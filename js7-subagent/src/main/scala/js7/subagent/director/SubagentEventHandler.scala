package js7.subagent.director

import cats.effect.IO
import cats.syntax.foldable.*
import fs2.Pipe
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.time.ScalaTime.*
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, EventId, KeyedEvent, Stamped, TsKeyedEvent}
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.subagent.SubagentItemStateEvent.SubagentEventsObserved
import js7.data.subagent.{SubagentDirectorState, SubagentEvent, SubagentId, SubagentItemStateEvent}
import js7.journal.{CommitOptions, Journal}
import js7.subagent.director.SubagentEventHandler.*
import scala.concurrent.duration.FiniteDuration
import scala.util.chaining.scalaUtilChainingOps

/** Handles the events of a local or remote Subagent and persists them in the Director's Journal.
  *
  * @param handleSpecialEvent handles local or remote Subagent-specific events,
  *                           before the common handling.
  */
private final class SubagentEventHandler(
  subagentId: SubagentId,
  journal: Journal[? <: SubagentDirectorState[?]],
  eventDelay: FiniteDuration,
  onOrderProcessed: (OrderId, OrderProcessed) => IO[Option[IO[Unit]]],
  releaseEvents: EventId => IO[Unit])
  (handleSpecialEvent: PartialFunction[Stamped[AnyKeyedEvent], IO[Handled]]):

  private val logger = Logger.withPrefix[SubagentEventHandler](subagentId.toString)
  private val commitOptions = CommitOptions(transaction = true, delay = eventDelay)

  /** Handles, persists and releases the events in chunks, then runs the follow-ups. */
  def pipe(bufferSize: Int): Pipe[IO, Stamped[AnyKeyedEvent], Unit] =
    _.pipe: stream =>
      if !eventDelay.isPositive then
        stream.chunks
      else
        stream.groupWithin(bufferSize, eventDelay)
    .evalMap:
      _.traverse(handleEvent)
        .flatMap: handledChunk =>
          val (updatedStampedMaybes, followUps) = handledChunk.toVector.unzip
          val updatedStampedSeq = updatedStampedMaybes.flatten
          updatedStampedSeq.lastOption.map(_.eventId).foldMapM: lastEventId =>
            journal.persistKeyedEvents(commitOptions):
              updatedStampedSeq.view.map: stamped =>
                TsKeyedEvent(stamped.value, epochMilli = stamped.timestamp.toEpochMilli)
              .appended:
                subagentId <-: SubagentEventsObserved(lastEventId)
            .map(_.orThrow /*???*/)
            .productR:
              // • After an OrderProcessed event, a ReleaseEvents command must be sent,
              //   to terminate StartOrderProcess command idempotency detection and
              //   to allow a new StartOrderProcess command for a next process.
              // • ReleaseEvents should also be sent to avoid Subagent's MemoryJournal overflow.
              // OPTIMISE: ReleaseEvents only after OrderProcessed,
              //  or (asynchronously) after a number of events
              releaseEvents(lastEventId)
          .productR:
            followUps.combineAll

  /** Returns optionally the event and a follow-up IO. */
  private def handleEvent(stamped: Stamped[AnyKeyedEvent]): IO[Handled] =
    handleSpecialEvent.applyOrElse(stamped, handleCommonEvent)

  private def handleCommonEvent(stamped: Stamped[AnyKeyedEvent]): IO[Handled] =
    stamped.value match
      case keyedEvent @ KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        event match
          case _: OrderStdWritten =>
            IO.pure(Some(stamped) -> IO.unit)

          case orderProcessed: OrderProcessed =>
            onOrderProcessed(orderId, orderProcessed).map:
              case None => None -> IO.unit  // OrderProcessed already handled
              case Some(followUp) =>
                // The followUp IO notifies OrderActor about OrderProcessed by calling `onEvents`
                Some(stamped) -> followUp

          case _ =>
            logger.error(s"Unexpected event: $keyedEvent")
            IO.pure(None -> IO.unit)

      case KeyedEvent(NoKey, SubagentEvent.SubagentShutdownStarted) =>
        IO.pure:
          Some(stamped.copy(value = subagentId <-: SubagentItemStateEvent.SubagentShutdownStarted))
            -> IO.unit

      case KeyedEvent(NoKey, event: SubagentEvent.SubagentItemAttached) =>
        logger.debug(event.toShortString)
        IO.pure(None -> IO.unit)

      case keyedEvent =>
        logger.error(s"Unexpected event: $keyedEvent")
        IO.pure(None -> IO.unit)

  override def toString = s"SubagentEventHandler($subagentId)"


private object SubagentEventHandler:
  /** Optionally the (translated) event to be persisted, and a follow-up IO. */
  type Handled = (Option[Stamped[AnyKeyedEvent]], IO[Unit])
