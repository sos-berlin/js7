package js7.controller.web.controller.api.fatevent

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives.{complete, get, pathEnd}
import akka.http.scaladsl.server.Route
import java.util.concurrent.Executors.newSingleThreadExecutor
import js7.base.auth.ValidUserPermission
import js7.base.time.ScalaTime._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.RichThrowable
import js7.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import js7.common.akkahttp.ConcurrentRequestLimiter
import js7.common.akkahttp.EventSeqStreamingSupport.NonEmptyEventSeqJsonStreamingSupport
import js7.common.akkahttp.StandardMarshallers._
import js7.common.event.EventWatch
import js7.common.event.collector.EventDirectives.{DefaultTimeout, eventRequest}
import js7.common.scalautil.Logger
import js7.controller.configuration.ControllerConfiguration
import js7.controller.data.events.{ControllerAgentEvent, ControllerEvent}
import js7.controller.problems.FatEventServiceBusyProblem
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.fatevent.FatEventRoute._
import js7.core.event.journal.watch.ClosedException
import js7.data.event.{Event, EventRequest, EventSeq, KeyedEvent, TearableEventSeq}
import js7.data.fatevent.FatEvent
import js7.data.filebased.RepoEvent
import js7.data.order.OrderEvent
import monix.eval.Task
import monix.execution.{Scheduler, UncaughtExceptionReporter}
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

// For tests see FatEventsTest

/**
  * @author Joacim Zschimmer
  */
trait FatEventRoute extends ControllerRouteProvider
{
  protected def eventWatch: EventWatch
  protected def controllerConfiguration: ControllerConfiguration

  private implicit val fatScheduler: Scheduler = Scheduler(
    newSingleThreadExecutor { runnable =>
      val thread = new Thread(runnable)
      thread.setName(s"JS7-fatEvent-${thread.getId}")
      thread
    },
    reporter = new UncaughtExceptionReporter {
      def reportFailure(throwable: Throwable): Unit =
        throwable match {
          case t: ClosedException => logger.debug(t.toString)  // Why throws thread?
          case throwable: Throwable => logger.error(throwable.toStringWithCauses, throwable)
        }
      })

  // Rebuilding FatState may take a long time, so we allow only one per time.
  // The client may impatiently abort and retry, overloading the server with multiple useless requests.
  private val concurrentRequestsLimiter = new ConcurrentRequestLimiter(limit = 1, FatEventServiceBusyProblem, timeout = 1.second, queueSize = 1)

  private lazy val fatStateCache = new FatStateCache(controllerConfiguration.controllerId, eventWatch)

  @TestOnly
  protected def isBusy = concurrentRequestsLimiter.isBusy

  final val fatEventRoute: Route =
    pathEnd {
      get {
        authorizedUser(ValidUserPermission) { _ =>
          eventRequest[FatEvent](defaultReturnType = Some("FatEvent")).apply { fatRequest =>
            concurrentRequestsLimiter(
              complete(requestFatEvents(fatRequest).runToFuture))
          }
        }
      }
    }

  private def requestFatEvents(fatRequest: EventRequest[FatEvent]): Task[ToResponseMarshallable] = {
    val deadline = now + fatRequest.timeout.getOrElse(DefaultTimeout)
    Task {
      fatStateCache.newAccessor(fatRequest.after) match {
        case None =>
          ToResponseMarshallable(TearableEventSeq.Torn(fatRequest.after): TearableEventSeq[Seq, KeyedEvent[FatEvent]])

        case Some(stateAccessor) =>
          def requestFat(underlyingRequest: EventRequest[Event]): Task[ToResponseMarshallable] =
            eventWatch.when[Event](underlyingRequest.copy[Event](timeout = Some(deadline.timeLeftOrZero)))
              .map(stateAccessor.toFatEventSeq(fatRequest, _))  // May take a long time !!!
              .map {
                case o: TearableEventSeq.Torn =>
                  ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[FatEvent]])

                case empty: EventSeq.Empty =>
                  stateAccessor.skipIgnoredEventIds(empty.lastEventId)
                  val nextTimeout = deadline.timeLeftOrZero
                  if (nextTimeout > Duration.Zero)
                    requestFat(underlyingRequest.copy[Event](after = empty.lastEventId)).runToFuture
                  else
                    ToResponseMarshallable(empty: TearableEventSeq[Seq, KeyedEvent[FatEvent]])

                case EventSeq.NonEmpty(stampedIterator) =>
                  implicit val x = NonEmptyEventSeqJsonStreamingSupport
                  closeableIteratorToMarshallable(stampedIterator)
              }

          requestFat(EventRequest[Event](
              Set(classOf[OrderEvent], classOf[RepoEvent], classOf[ControllerEvent], classOf[ControllerAgentEvent.AgentReady]),
              after = stateAccessor.eventId,
              timeout = fatRequest.timeout,
              delay = fatRequest.delay,
              limit = Int.MaxValue))
            .runToFuture
      }
    }
  }
}

object FatEventRoute {
  intelliJuseImport(Scheduler)
  private val logger = Logger(getClass)
}
