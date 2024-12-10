package js7.base.io.process

import cats.effect.IO
import java.io.IOException
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.FiniteDuration


object StartRobustly:

  private val logger = Logger[this.type]
  private val DefaultDurations = List(10.ms, 50.ms, 500.ms, 1440.ms)
  assert(DefaultDurations.map(_.toMillis).sum.ms == 2.s)

  extension (processBuilder: ProcessBuilder)
    /**
     * Like ProcessBuilder.start, but retries after IOException("error=26, Text file busy").
     *
     * @see https://change.sos-berlin.com/browse/JS-1581
     * @see https://bugs.openjdk.java.net/browse/JDK-8068370
     */
    def startRobustly(durations: Iterable[FiniteDuration] = DefaultDurations): IO[Process] =
      IO.defer:
        val durationsIterator = durations.iterator
        IO.blocking:
          processBuilder.start()
        .onErrorRestartLoop(()):
          case (TextFileBusyIOException(e), _, restart) if durationsIterator.hasNext =>
            logger.warn(s"Retrying process start after error: ${e.toString}")
            restart(()).delayBy(durationsIterator.next())

          case (throwable, _, _) =>
            IO.raiseError(throwable)


  private[process] object TextFileBusyIOException:
    private def matchError26(o: String) =
    """.*\berror=26\b.*""".r.pattern.matcher(o)

    def unapply(e: IOException): Option[IOException] =
      matchError26(Option(e.getMessage) getOrElse "").matches ? e
