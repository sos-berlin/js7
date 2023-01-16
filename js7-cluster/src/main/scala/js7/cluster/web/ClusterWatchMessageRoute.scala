package js7.cluster.web

import akka.http.scaladsl.marshalling.ToResponseMarshallable
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
import js7.cluster.web.ClusterWatchMessageRoute.*
import js7.common.akkahttp.AkkaHttpServerUtils.{accept, observableToResponseMarshallable}
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.akkahttp.web.session.RouteProvider
import js7.common.http.JsonStreamingSupport.{NdJsonStreamingSupport, `application/x-ndjson`, jsonSeqMarshaller}
import js7.data.cluster.{ClusterState, ClusterWatchMessage}
import js7.data.event.Stamped
import js7.data.node.NodeId
import js7.journal.watch.EventWatch
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.reactivestreams.Publisher
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NoStackTrace

trait ClusterWatchMessageRoute extends RouteProvider
{
  protected def scheduler: Scheduler

  private implicit def implicitScheduler: Scheduler = scheduler

  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)

  protected def checkedClusterState: Task[Checked[Stamped[ClusterState]]]
  protected def clusterWatchMessageStream: Task[Checked[Stream[Task, ClusterWatchMessage]]]
  protected def eventWatch: EventWatch
  protected def nodeId: NodeId

  protected final def clusterWatchMessageRoute(userId: UserId): Route =
    Route.seal(
      accept(`application/x-ndjson`)(
        extractRequest(request =>
          parameter("keepAlive".as[FiniteDuration]) { keepAlive =>
            complete {
              stream
                .flatMapT { stream =>
                  val observable = Observable.fromReactivePublisher(
                    stream.toUnicastPublisher: Publisher[ClusterWatchMessage])
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
            }
          })))

  private def stream: Task[Checked[Stream[Task, ClusterWatchMessage]]] =
    clusterWatchMessageStream
      .map(_.map(_.handleErrorWith { throwable =>
        // The streaming event web service doesn't have an error channel,
        // so we simply end the stream
        logger.warn(throwable.toStringWithCauses)
        if (throwable.getStackTrace.nonEmpty) logger.debug(throwable.toStringWithCauses, throwable)
        Stream.empty
      }))

  private val emptyResponseMarshallable: ToResponseMarshallable =
    observableToMarshallable(Observable.empty)

  private def observableToMarshallable(observable: Observable[ClusterWatchMessage])
  : ToResponseMarshallable = {
    implicit val x = NdJsonStreamingSupport
    implicit val y = jsonSeqMarshaller[ClusterWatchMessage]
    monixObservableToMarshallable(
      observable
        .takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ => Task {
          logger.debug("whenShuttingDown completed")
        }))
  }
}

object ClusterWatchMessageRoute
{
  private val logger = Logger[this.type]

  private object CanceledException extends Exception with NoStackTrace
}