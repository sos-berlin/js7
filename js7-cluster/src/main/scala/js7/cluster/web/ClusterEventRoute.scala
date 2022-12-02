package js7.cluster.web

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.HttpEntity.Chunk
import akka.http.scaladsl.model.{HttpEntity, HttpRequest}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.util.ByteString
import fs2.Stream
import fs2.interop.reactivestreams.*
import io.circe.syntax.EncoderOps
import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.FutureCompletion
import js7.base.utils.FutureCompletion.syntax.{FutureCompletionFuture, FutureCompletionObservable}
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.web.ClusterEventRoute.*
import js7.common.akkahttp.AkkaHttpServerUtils.accept
import js7.common.akkahttp.ByteSequenceChunkerObservable.syntax.RichByteSequenceChunkerObservable
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.akkahttp.web.session.RouteProvider
import js7.common.akkautils.ByteStrings.syntax.*
import js7.common.http.JsonStreamingSupport.{NdJsonStreamingSupport, `application/x-ndjson`, jsonSeqMarshaller}
import js7.common.http.StreamingSupport.AkkaObservable
import js7.data.cluster.{ClusterState, ClusterWatchMessage}
import js7.data.event.Stamped
import js7.data.node.NodeId
import js7.journal.watch.EventWatch
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.reactivestreams.Publisher
import scala.concurrent.duration.FiniteDuration
import scala.util.control.{NoStackTrace, NonFatal}

trait ClusterEventRoute extends RouteProvider
{
  protected def scheduler: Scheduler

  private implicit def implicitScheduler: Scheduler = scheduler

  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)

  protected def checkedClusterState: Task[Checked[Stamped[ClusterState]]]
  protected def clusterWatchMessageStream: Task[Stream[Task, ClusterWatchMessage]]
  protected def eventWatch: EventWatch
  protected def nodeId: NodeId

  protected final def clusterEventRoute(userId: UserId): Route =
    Route.seal(
      accept(`application/x-ndjson`)(
        extractRequest(request =>
          parameter("heartbeat".as[FiniteDuration]) { heartbeat =>
            complete {
              stream
                .flatMap { stream =>
                  val observable = Observable.fromReactivePublisher(
                    stream.toUnicastPublisher: Publisher[ClusterWatchMessage])
                  val toResponseMarshallable = observableToResponseMarshallable(
                    observable, request, userId)
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

  //private def heartbeatingObservable(heartbeat: FiniteDuration)
  private def stream: Task[/*Checked[*/Stream[Task, ClusterWatchMessage]] =
//    checkedClusterState.map(_.map {
//      case Stamped(_, _, initialClusterState) =>
//        //var _clusterState = initialClusterState
//        val initialMsg = initialClusterState match {
//          case ClusterState.Empty => None
//          case o: HasNodes => Some(ClusterWatchHeartbeat(CorrelId.current, nodeId, o))
//        }
        clusterWatchMessageStream
          .map(_
          // FIXME Ist der Übergang von initialClusterState zum Stream nahtlos?
          //.startWith(initialMsg.toList)
          // Oder .echoRepeated() ???
          //.insertHeartbeatsOnSlowUpstream(
          //  heartbeat,
          //  ClusterWatchMessage(nodeId, ClusterWatchHeartbeat, _clusterState))
            .handleErrorWith {
              case NonFatal(e) =>
                // The streaming event web service doesn't have an error channel,
                // so we simply end the stream
                logger.warn(e.toStringWithCauses)
                if (e.getStackTrace.nonEmpty) logger.debug(e.toStringWithCauses, e)
                Stream.empty
              case e => Stream.raiseError[Task](e)
            })
          //eventWatch
          //  .observe(EventRequest.singleClass[ClusterEvent](
          //    after = initialEventId, timeout = None, delay = 0.s))
          //  .map { case Stamped(eventId, _, KeyedEvent(NoKey, event)) =>
          //    _eventId = eventId
          //    val updatedClusterState = _clusterState.applyEvent(event).orThrow.asInstanceOf[HasNodes]
          //    _clusterState = updatedClusterState
          //    Stamped(eventId, ClusterWatchMessage(event, updatedClusterState))
          //  }
          //  .insertHeartbeatsOnSlowUpstream(
          //    heartbeat,
          //    Stamped(_eventId, ClusterWatchMessage(ClusterWatchHeartbeat, _clusterState)))
          //  .onErrorRecoverWith { case NonFatal(e) =>
          //    // The streaming event web service doesn't have an error channel,
          //    // so we simply end the stream
          //    logger.warn(e.toStringWithCauses)
          //    if (e.getStackTrace.nonEmpty) logger.debug(e.toStringWithCauses, e)
          //    Observable.empty
          //  }
    //}

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

  private def waitUntilClusterStateIsNotEmpty: Task[ClusterWatchMessage] = {
    ???
  }

  private def observableToResponseMarshallable(
    observable: Observable[ClusterWatchMessage],
    httpRequest: HttpRequest,
    userId: UserId)
  : ToResponseMarshallable =
    ToResponseMarshallable(
      HttpEntity.Chunked(
        `application/x-ndjson`,
        observable
          .map(_.asJson.toByteSequence[ByteString] ++ LF)
          .chunk(chunkSize)
          .map(Chunk(_))
          // TODO Delay takeUntil until no more events are available (time limited)
          //  to let flow queued events to the client before shutdown.
          .takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ => Task {
            logger.debug(s"Shutdown observing events for $userId ${httpRequest.uri}")
          })
          .toAkkaSourceForHttpResponse))}

object ClusterEventRoute
{
  private val logger = Logger[this.type]
  private val LF = ByteString("\n")

  private object CanceledException extends Exception with NoStackTrace
}
