package js7.common.http

import org.apache.pekko
import org.apache.pekko.NotUsed
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.{Sink, Source}
import cats.effect.ExitCase
import com.typesafe.scalalogging.Logger
import izumi.reflect.Tag
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.common.pekkohttp.ExceptionHandling.webLogger
import cats.effect.IO
import monix.execution.Scheduler
import fs2.Stream


/**
  * @author Joacim Zschimmer
  */
object StreamingSupport:
  private val logger = Logger("js7.common.http.StreamingSupport")  // TODO Use Logger adapter (unreachable in module common)

  implicit final class PekkoStream[A](private val stream: Stream[IO, A]) extends AnyVal:
    def toPekkoSourceForHttpResponse(implicit scheduler: Scheduler, A: Tag[A]): Source[A, NotUsed] =
      logPekkoStreamErrorToWebLogAndIgnore(toPekkoSource)

    def toPekkoSourceIO(implicit A: Tag[A]): IO[Source[A, NotUsed]] =
      IO.deferAction(implicit scheduler => IO(
        toPekkoSource(scheduler, A)))

    def toPekkoSource(implicit scheduler: Scheduler, A: Tag[A]): Source[A, NotUsed] =
      Source.fromPublisher(
        stream
          .guaranteeCase {
            case ExitCase.Completed => IO.unit
            case exitCase => IO { logger.trace(s"Stream[IO, ${A.tag}] toPekkoSource: $exitCase") }
          }
          .toReactivePublisher(scheduler))

  def logPekkoStreamErrorToWebLogAndIgnore[A: Tag](source: Source[A, NotUsed]): Source[A, NotUsed] =
    source.recoverWithRetries(1, { case throwable =>
      // These are the only messages logged
      val isDebug = throwable.isInstanceOf[pekko.stream.AbruptTerminationException]
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

  implicit final class StreamPekkoSource[Out, Mat](private val source: Source[Out, Mat]) extends AnyVal:
    def toStream(implicit m: Materializer): Stream[IO, Out] =
      Stream
        .fromReactivePublisher(source.runWith(Sink.asPublisher(fanout = false)))
        .guaranteeCase:
          case ExitCase.Completed => IO.unit
          case exitCase => IO { logger.trace(s"toStream: $exitCase") }
