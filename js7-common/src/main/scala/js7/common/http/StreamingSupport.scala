package js7.common.http

import cats.effect
import cats.effect.Resource.ExitCase
import cats.effect.{Deferred, IO, Resource, ResourceIO}
import fs2.Stream
import fs2.interop.reactivestreams.{PublisherOps, StreamOps}
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.startAndForget
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
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

    def toPekkoSourceForHttpResponseX(using Tag[A]): IO[Source[A, NotUsed]] =
      Deferred[IO, IO[Unit]].flatMap: deferredRelease =>
        stream
          .onFinalizeCase: exitCase =>
            deferredRelease.get
              .flatMap: release =>
                //logger
                //  .traceIO(s"toPekkoSourceForHttpResponseX deferred release: ${exitCase.toOutcome[IO]}"):
                release.logWhenItTakesLonger("toPekkoSourceForHttpResponseX.release")
              // Delay needed. Otherwise, the Pekko stream may not be complete !!!
              // Does this relate to
              // - pekko.http.(client|server).stream-cancellation-delay ?
              // - org.apache.pekko.stream.CancellationStrategy.AfterDelay ?
              .delayBy(100.ms)
              .startAndForget // Release in asynchronously, otherwise we will stick in a deadlock !!!
          .toPekkoSourceForHttpResponse
          .allocated
          .flatMap: (source, release) =>
            deferredRelease.complete(release)
              .as(source)

    def toPekkoSourceForHttpResponse(using A: Tag[A]): ResourceIO[Source[A, NotUsed]] =
      //logger.traceResource:
        stream
          .handleErrorWith: throwable =>
            logStreamError(throwable)
            Stream.empty
          .toPekkoSourceResource.map(logPekkoStreamErrorToWebLogAndIgnore)

    def toPekkoSourceResource: ResourceIO[Source[A, NotUsed]] =
      stream
        .onFinalizeCase:
          case ExitCase.Canceled => IO(logger.debug(s"◼️  toPekkoSourceResource stream canceled"))
          case _ => IO.unit
        .toUnicastPublisher
        .map(Source.fromPublisher)

    private def logPekkoStreamErrorToWebLogAndIgnore(source: Source[A, NotUsed])(using Tag[A])
    : Source[A, NotUsed] =
      source.recoverWithRetries(1, throwable =>
        logStreamError(throwable)

        // Letting the throwable pass would close the connection,
        // and the HTTP client sees only: The request's encoding is corrupt:
        // The connection closed with error: Connection reset by peer.
        // => So it seems best to end the stream silently.
        Source.empty)

    private def logStreamError(throwable: Throwable)(using tag: Tag[A]): Unit =
      // These are the only messages logged
      val isDebug = throwable.isInstanceOf[pekko.stream.AbruptTerminationException]
      val msg = s"Terminating Source[${tag.tag}] stream due to error: ${throwable.toStringWithCauses}"
      if isDebug then
        webLogger.debug(msg)
      else
        webLogger.warn(msg)
      if throwable.getStackTrace.nonEmpty then
        logger.debug(msg, throwable)


  extension [Out, Mat](source: Source[Out, Mat])
    def asFs2Stream(bufferSize: Int = 1)(using Tag[Out], Materializer): Stream[IO, Out] =
      logger.traceStream:
        Stream.suspend:
          val publisher = source.runWith(Sink.asPublisher(fanout = false))
          publisher.toStreamBuffered[IO](bufferSize = bufferSize)
