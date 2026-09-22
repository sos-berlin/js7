package js7.base.log

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.traverse.*
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.log.AnsiEscapeCodes.bold
import js7.base.log.Log4jTest.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.{DurationRichInt, RichDeadline, sleep}
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.base.utils.Tests.isIntelliJIdea
import scala.concurrent.duration.Deadline.now

/* To speed-test Log4jThreadContextMap, insert %X{js7.prettyVersion} into log4j2.xml patternLayout !!!

2026-09-22 MacBook Pro M4 log output, with %X{js7.prettyVersion}, best of five iterations, log4j 2.26.1
  — Times varies, could be half —
  0.21s/million lines (⌀0.21µs), ~4'662'571 lines/s
  0.21s/0.3 GB, ~1.5 GB/s
  0 CorrelIds generated, 0× string, 0× bindCorrelId, 0× CorrelId.current
*/
final class Log4jTest extends OurTestSuite:

  private val testSpeed = isIntelliJIdea
  private given IORuntime = ioRuntime

  "Speed" in:
    if !testSpeed then
      doTestSpeed(3, 3, "")
    else
      (1 to 5).foreach: i =>
        doTestSpeed(1000, 1000, s"$i:")
        sleep(500.ms)
      //logger.info(CorrelId.statistics)
      //logger.info(Log4jThreadContextMap.statistics)

  private def doTestSpeed(n: Int, m: Int, label: String): Unit =
    val started = now
    (1 to n).toVector.traverse: i =>
      CorrelId.bindNew:
        IO:
          var j = 1
          while j <= m do
            logger.debug(s"$i-$j $Line")
            j += 1
    .await(99.s)

    val elapsed = started.elapsed
    logger.info(label + " " + bold(itemsPerSecondString(elapsed, n * m, "lines")))
    logger.info(label + " " + bold(bytesPerSecondString(elapsed, n.toLong * m * EstimatedByteCount)))


object Log4jTest:
  private lazy val logger = Logger[this.type]
  private val Line = " long" * 50
  private val EstimatedByteCount =
    40 + this.getClass.scalaName.length + 6 + Line.getBytes(UTF_8).length
