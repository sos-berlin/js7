package js7.cluster.web

import fs2.Stream
import fs2.interop.reactivestreams.*
import js7.base.auth.UserId
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.FutureCompletion
import js7.base.utils.FutureCompletion.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.web.ClusterWatchRequestRoute.*
import js7.common.pekkohttp.PekkoHttpServerUtils.{accept, streamToResponseMarshallable}
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkohttp.web.session.RouteProvider
import js7.common.http.JsonStreamingSupport.{NdJsonStreamingSupport, `application/x-ndjson`, jsonSeqMarshaller}
import js7.common.pekkohttp.PekkoHttpServerUtils.{accept, observableToResponseMarshallable}
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkohttp.web.session.RouteProvider
import js7.data.cluster.{ClusterState, ClusterWatchRequest}
import js7.data.event.Stamped
import js7.data.node.NodeId
import js7.journal.watch.EventWatch
import cats.effect.IO
import monix.execution.Scheduler
import monix.reactive.Observable
import org.apache.pekko.http.scaladsl.common.JsonEntityStreamingSupport
import org.apache.pekko.http.scaladsl.marshalling.{ToEntityMarshaller, ToResponseMarshallable}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import fs2.Stream
import org.reactivestreams.Publisher
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NoStackTrace

trait ClusterWatchRequestRoute extends RouteProvider:
  protected def scheduler: Scheduler

  private implicit def implicitScheduler: Scheduler = scheduler

  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)

  protected def checkedClusterState: IO[Checked[Stamped[ClusterState]]]
  protected def clusterWatchRequestStream: IO[fs2.Stream[IO, ClusterWatchRequest]]
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
                  val stream = Stream.fromReactivePublisher(
                    stream.toUnicastPublisher: Publisher[ClusterWatchRequest])
                  val toResponseMarshallable = streamToResponseMarshallable(
                    stream, request, userId, whenShuttingDownCompletion,
                    keepAlive = Some(keepAlive),
                    chunkSize = chunkSize)
                  IO(toResponseMarshallable)
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
    streamToMarshallable(Stream.empty)

  private def streamToMarshallable(stream: Stream[IO, ClusterWatchRequest])
  : ToResponseMarshallable =
    implicit val x: JsonEntityStreamingSupport = NdJsonStreamingSupport
    implicit val y: ToEntityMarshaller[ClusterWatchRequest] = jsonSeqMarshaller[ClusterWatchRequest]
    monixStreamToMarshallable(
      stream
        .takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ => IO {
          logger.debug("whenShuttingDown completed")
        }))


object ClusterWatchRequestRoute:
  private val logger = Logger[this.type]

  private object CanceledException extends Exception, NoStackTrace
