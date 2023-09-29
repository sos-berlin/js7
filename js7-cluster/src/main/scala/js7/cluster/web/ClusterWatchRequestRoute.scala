package js7.cluster.web

import akka.http.scaladsl.common.JsonEntityStreamingSupport
import akka.http.scaladsl.marshalling.{ToEntityMarshaller, ToResponseMarshallable}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import fs2.Stream
import fs2.interop.reactivestreams.*
import js7.base.auth.UserId
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.FutureCompletion
import js7.base.utils.FutureCompletion.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.web.ClusterWatchRequestRoute.*
import js7.common.akkahttp.AkkaHttpServerUtils.{accept, observableToResponseMarshallable}
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.akkahttp.web.session.RouteProvider
import js7.common.http.JsonStreamingSupport.{NdJsonStreamingSupport, `application/x-ndjson`, jsonSeqMarshaller}
import js7.data.cluster.{ClusterState, ClusterWatchRequest}
import js7.data.event.Stamped
import js7.data.node.NodeId
import js7.journal.watch.EventWatch
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.reactivestreams.Publisher
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NoStackTrace

trait ClusterWatchRequestRoute extends RouteProvider
{
  protected def scheduler: Scheduler

  private implicit def implicitScheduler: Scheduler = scheduler

  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)

  protected def checkedClusterState: Task[Checked[Stamped[ClusterState]]]
  protected def clusterWatchRequestStream: Task[fs2.Stream[Task, ClusterWatchRequest]]
  protected def eventWatch: EventWatch
  protected def nodeId: NodeId

  protected final def clusterWatchMessageRoute(userId: UserId): Route =
    Route.seal(
      accept(`application/x-ndjson`)(
        extractRequest(request =>
          parameter("keepAlive".as[FiniteDuration])(keepAlive =>
            complete {
              clusterWatchRequestStream
                .map(_
                  .handleErrorWith { throwable =>
                    // The streaming event web service doesn't have an error channel,
                    // so we simply end the stream
                    logger.warn(throwable.toStringWithCauses)
                    if throwable.getStackTrace.nonEmpty then logger.debug(throwable.toStringWithCauses, throwable)
                    Stream.empty
                  })
                .flatMap { stream =>
                  val observable = Observable.fromReactivePublisher(
                    stream.toUnicastPublisher: Publisher[ClusterWatchRequest])
                  val toResponseMarshallable = observableToResponseMarshallable(
                    observable, request, userId, whenShuttingDownCompletion,
                    keepAlive = Some(keepAlive),
                    chunkSize = chunkSize)
                  Task(toResponseMarshallable)
                    .onCancelRaiseError(CanceledException)
                    .onErrorRecover {
                      case CanceledException => emptyResponseMarshallable
                    }
                    .map(Right(_))
                }
                .runToFuture
                .cancelOnCompletionOf(whenShuttingDownCompletion)
            }))))

  private val emptyResponseMarshallable: ToResponseMarshallable =
    observableToMarshallable(Observable.empty)

  private def observableToMarshallable(observable: Observable[ClusterWatchRequest])
  : ToResponseMarshallable = {
    implicit val x: JsonEntityStreamingSupport = NdJsonStreamingSupport
    implicit val y: ToEntityMarshaller[ClusterWatchRequest] = jsonSeqMarshaller[ClusterWatchRequest]
    monixObservableToMarshallable(
      observable
        .takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ => Task {
          logger.debug("whenShuttingDown completed")
        }))
  }
}

object ClusterWatchRequestRoute
{
  private val logger = Logger[this.type]

  private object CanceledException extends Exception with NoStackTrace
}
