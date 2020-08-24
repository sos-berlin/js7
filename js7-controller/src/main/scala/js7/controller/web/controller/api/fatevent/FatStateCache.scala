package js7.controller.web.controller.api.fatevent

import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.CloseableIterator
import js7.base.utils.ScalaUtils.syntax._
import js7.common.event.EventWatch
import js7.common.scalautil.Logger
import js7.controller.data.ControllerState
import js7.controller.web.controller.api.fatevent.FatStateCache._
import js7.data.controller.ControllerId
import js7.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import js7.data.fatevent.FatEvent
import monix.eval.Task
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/** Remembers two `FatState` of (1) last requested and (2) last returned EventId.
  */
private[fatevent] final class FatStateCache(controllerId: ControllerId, eventWatch: EventWatch)
{
  // May be accessed by multiple clients simultaneously
  @volatile
  private var lastRequested: Option[FatState] = None
  @volatile
  private var lastDelivered: Option[FatState] = None  // Modified while on-the-fly built FatEvent stream is being sent to client !!!

  def newAccessor(after: EventId): Option[Task[Accessor]] =
    fatStateFor(after).map(_.map(fatState => new Accessor(fatState, after)))

  private def fatStateFor(after: EventId): Option[Task[FatState]] =
    useFatState(after).map(Task.pure) orElse recoverFatState(after)

  private def useFatState(after: EventId): Option[FatState] =
    lastDelivered match {
      case Some(fatState) if fatState.eventId <= after =>
        logger.trace(s"Using last lastDelivered FatState ${EventId.toString(fatState.eventId)}" +
          ((after > fatState.eventId) ?? s", after=${EventId.toString(after)}"))
        Some(fatState)

      case _ =>
        lastRequested match {
          case Some(fatState) if fatState.eventId <= after =>
            logger.trace(s"Using last requested FatState ${EventId.toString(fatState.eventId)}" +
              ((after > fatState.eventId) ?? s", after=${EventId.toString(after)}"))
            Some(fatState)

          case _ =>
            None
        }
    }

  private def recoverFatState(after: EventId): Option[Task[FatState]] =
    eventWatch.snapshotAfter(after = after).map(snapshotObservable =>
      ControllerState.fromObservable(snapshotObservable)
        .map(controllerState =>
          FatState(controllerId, controllerState.eventId, controllerState.repo, controllerState.idToOrder)))

  final class Accessor(initialFatState: FatState, after: EventId)
  {
    private val initialEventId = initialFatState.eventId
    private var fatState = initialFatState

    def eventId = fatState.eventId

    def skipIgnoredEventIds(eventId: EventId): Unit = {
      assertThat(fatState.eventId <= eventId)
      fatState = fatState.copy(eventId = eventId)
      lastDelivered = Some(fatState)
    }

    def toFatEventSeq(
      request: EventRequest[FatEvent],
      eventSeq: TearableEventSeq[CloseableIterator, KeyedEvent[Event]])
    : TearableEventSeq[CloseableIterator, KeyedEvent[FatEvent]]
    =
      eventSeq match {
        case o: TearableEventSeq.Torn => o
        case o: EventSeq.Empty => o
        case EventSeq.NonEmpty(stampedIterator) =>
          var lastEventId = after
          val fatCloseableIterator = stampedIterator
            .flatMap { stamped =>
              lastEventId = stamped.eventId
              watch(stamped.eventId)  // Watch drop events used for FatState's rebuild
              toFatEvents(stamped)
            }
            .dropWhile { stamped =>
              val drop = stamped.eventId <= request.after
              drop
            }
            .take(request.limit)
          if (fatCloseableIterator.isEmpty)
            EventSeq.Empty(lastEventId)
          else
            EventSeq.NonEmpty(fatCloseableIterator)
      }

    private def toFatEvents(stamped: Stamped[KeyedEvent[Event]]): Option[Stamped[KeyedEvent[FatEvent]]] = {
      if (fatState.eventId <= after) {  // Still rebuilding?
        lastRequested = Some(fatState)
      }
      val (updated, fatEvents) = fatState.toFatEvents(stamped)
      fatState = updated
      lastDelivered = Some(fatState)
      fatEvents
    }

    private val runningSince = now
    private var eventCount = 0
    private var stillRebuildingLogged = false
    private var longDurationLogged = false

    private def watch(eventId: EventId): Unit = {
      eventCount += 1
      lazy val duration = runningSince.elapsed
      if (!stillRebuildingLogged && eventId <= after && duration >= InfoAfter) {
        stillRebuildingLogged = true
        logger.info(s"Still rebuilding requested FatState, $eventCount events processed since ${duration.pretty}")
      }
      if (eventId == after && eventCount > 0) {
        lazy val msg = s"FatState rebuilt from $eventCount events ${EventId.toString(initialEventId)}...${EventId.toString(after)} in ${duration.pretty}"
        if (!longDurationLogged && duration >= 30.seconds) {
          longDurationLogged = true
          logger.info(msg)
        } else {
          logger.debug(msg)
        }
      }
    }
  }
}

object FatStateCache {
  private val InfoAfter = 30.seconds
  private val logger = Logger(getClass)
}
