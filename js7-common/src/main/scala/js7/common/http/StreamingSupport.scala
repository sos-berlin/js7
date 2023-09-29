package js7.common.http

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.effect.ExitCase
import com.typesafe.scalalogging.Logger
import izumi.reflect.Tag
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.common.akkahttp.ExceptionHandling.webLogger
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable


/**
  * @author Joacim Zschimmer
  */
object StreamingSupport
{
  private val logger = Logger("js7.common.http.StreamingSupport")  // TODO Use Logger adapter (unreachable in module common)

  implicit final class AkkaObservable[A](private val observable: Observable[A]) extends AnyVal
  {
    def toAkkaSourceForHttpResponse(implicit scheduler: Scheduler, A: Tag[A]): Source[A, NotUsed] =
      logAkkaStreamErrorToWebLogAndIgnore(toAkkaSource)

    def toAkkaSourceTask(implicit A: Tag[A]): Task[Source[A, NotUsed]] =
      Task.deferAction(implicit scheduler => Task(
        toAkkaSource(scheduler, A)))

    def toAkkaSource(implicit scheduler: Scheduler, A: Tag[A]): Source[A, NotUsed] =
      Source.fromPublisher(
        observable
          .guaranteeCase {
            case ExitCase.Completed => Task.unit
            case exitCase => Task { logger.trace(s"Observable[${A.tag}] toAkkaSource: $exitCase") }
          }
          .toReactivePublisher(scheduler))
  }

  def logAkkaStreamErrorToWebLogAndIgnore[A: Tag](source: Source[A, NotUsed]): Source[A, NotUsed] =
    source.recoverWithRetries(1, { case throwable =>
      // These are the only messages logged
      val isDebug = throwable.isInstanceOf[akka.stream.AbruptTerminationException]
      val msg = s"Terminating Source[${implicitly[Tag[A]].tag}] stream due to error: ${throwable.toStringWithCauses}"
      if isDebug then webLogger.debug(msg)
      else webLogger.warn(msg)
      if throwable.getStackTrace.nonEmpty then logger.debug(msg, throwable)

      // Letting the throwable pass would close the connection,
      // and the HTTP client sees only: The request's encoding is corrupt:
      // The connection closed with error: Connection reset by peer.
      // => So it seems best to end the stream silently.
      Source.empty
    })

  implicit final class ObservableAkkaSource[Out, Mat](private val source: Source[Out, Mat]) extends AnyVal
  {
    def toObservable(implicit m: Materializer): Observable[Out] =
      Observable
        .fromReactivePublisher(source.runWith(Sink.asPublisher(fanout = false)))
        .guaranteeCase {
          case ExitCase.Completed => Task.unit
          case exitCase => Task { logger.trace(s"toObservable: $exitCase") }
        }
  }
}
