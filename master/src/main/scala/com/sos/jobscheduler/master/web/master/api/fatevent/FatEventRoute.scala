package com.sos.jobscheduler.master.web.master.api.fatevent

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives.{complete, get, pathEnd}
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport.jsonOrYamlMarshaller
import com.sos.jobscheduler.common.akkahttp.ConcurrentRequestLimiter
import com.sos.jobscheduler.common.akkahttp.EventSeqStreamingSupport.NonEmptyEventSeqJsonStreamingSupport
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.common.event.collector.EventDirectives.{DefaultTimeout, eventRequest}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.watch.ClosedException
import com.sos.jobscheduler.core.problems.FatEventServiceBusyProblem
import com.sos.jobscheduler.data.event.{Event, EventRequest, EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.fatevent.FatEvent
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.master.data.events.{MasterAgentEvent, MasterEvent}
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import com.sos.jobscheduler.master.web.master.api.fatevent.FatEventRoute._
import java.util.concurrent.Executors.newSingleThreadExecutor
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Seq
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

// For tests see FatEventsTest

/**
  * @author Joacim Zschimmer
  */
trait FatEventRoute extends MasterRouteProvider
{
  protected def eventWatch: EventWatch[Event]

  private implicit val fatScheduler = Scheduler(
    newSingleThreadExecutor { runnable =>
      val thread = new Thread(runnable)
      thread.setName(s"JobScheduler-fatEvent-${thread.getId}")
      thread
    },
    _ match {
      case t: ClosedException => logger.debug(t.toString)  // Why throws thread?
      case throwable: Throwable => logger.error(throwable.toStringWithCauses, throwable)
    })

  // Rebuilding FatState may take a long time, so we allow only one per time.
  // The client may impatiently abort and retry, overloading the server with multiple useless requests.
  private val concurrentRequestsLimiter = new ConcurrentRequestLimiter(limit = 1, FatEventServiceBusyProblem, timeout = 1.second, queueSize = 1)

  private lazy val fatStateCache = new FatStateCache(eventWatch)

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
    val timeoutAt = now + fatRequest.timeout.getOrElse(DefaultTimeout)
    Task {
      val stateAccessor = fatStateCache.newAccessor(fatRequest.after)

      def requestFat(underlyingRequest: EventRequest[Event]): Task[ToResponseMarshallable] =
        eventWatch.when[Event](underlyingRequest.copy[Event](timeout = Some(timeoutAt - now)))
          .map(stateAccessor.toFatEventSeq(fatRequest, _))  // May take a long time !!!
          .map {
            case o: TearableEventSeq.Torn =>
              ToResponseMarshallable(o: TearableEventSeq[Seq, KeyedEvent[FatEvent]])

            case empty: EventSeq.Empty =>
              stateAccessor.skipIgnoredEventIds(empty.lastEventId)
              val nextTimeout = timeoutAt - now
              if (nextTimeout > Duration.Zero)
                requestFat(underlyingRequest.copy[Event](after = empty.lastEventId)).runToFuture
              else
                ToResponseMarshallable(empty: TearableEventSeq[Seq, KeyedEvent[FatEvent]])

            case EventSeq.NonEmpty(stampedIterator) =>
              implicit val x = NonEmptyEventSeqJsonStreamingSupport
              closeableIteratorToMarshallable(stampedIterator)
          }

      requestFat(EventRequest[Event](
          Set(classOf[OrderEvent], classOf[RepoEvent], classOf[MasterEvent], classOf[MasterAgentEvent.AgentReady]),
          after = stateAccessor.eventId,
          timeout = fatRequest.timeout,
          delay = fatRequest.delay,
          limit = Int.MaxValue))
        .runToFuture
    }
  }
}

object FatEventRoute {
  intelliJuseImport(Scheduler)
  private val logger = Logger(getClass)
}
