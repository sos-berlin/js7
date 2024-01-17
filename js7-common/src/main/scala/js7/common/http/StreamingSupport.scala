package js7.common.http

import cats.effect
import cats.effect.{IO, Resource}
import fs2.Stream
import fs2.interop.reactivestreams.*
import izumi.reflect.Tag
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.common.pekkohttp.ExceptionHandling.webLogger
import org.apache.pekko
import org.apache.pekko.NotUsed
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.{Sink, Source}

/**
  * @author Joacim Zschimmer
  */
object StreamingSupport:

  private val logger = Logger[this.type]

  extension [A](stream: Stream[IO, A])
    def toPekkoSourceForHttpResponse(using A: Tag[A]): Resource[IO, Source[A, NotUsed]] =
      toPekkoSourceResource.map(logPekkoStreamErrorToWebLogAndIgnore)

    def toPekkoSourceResource(using A: Tag[A]): Resource[IO, Source[A, NotUsed]] =
      stream
        .onFinalizeCase:
          case Resource.ExitCase.Succeeded => IO.unit
          case exitCase => IO(logger.trace(s"Stream[IO, ${A.tag}] toPekkoSource => $exitCase"))
        .toUnicastPublisher
        .map(Source.fromPublisher)

  def logPekkoStreamErrorToWebLogAndIgnore[A: Tag](source: Source[A, NotUsed]): Source[A, NotUsed] =
    source.recoverWithRetries(1, throwable =>
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
      Source.empty)


  extension [Out, Mat](source: Source[Out, Mat])
    def toFs2Stream(implicit m: Materializer): Stream[IO, Out] =
      val publisher = source.runWith(Sink.asPublisher(fanout = false))
      fromPublisher[IO, Out](publisher, bufferSize = 1)
        .onFinalizeCase:
          case Resource.ExitCase.Succeeded => IO.unit
          case exitCase => IO { logger.trace(s"toStream: $exitCase") }
