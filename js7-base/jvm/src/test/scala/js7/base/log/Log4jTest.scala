package js7.base.log

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.traverse.*
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.log.Log4jTest.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.{DurationRichInt, RichDeadline, sleep}
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import scala.concurrent.duration.Deadline.now

/* To speed-test Log4jThreadContextMap, insert %X{js7.version} into log4j2.xml patternLayout !!!

2024-10-07 MacBook pro M1 log output, with %X{js7.version}:
  0.75s/1000000 lines (⌀0.75µs), ~1337127 lines/s
  0.75s/318 MB, ~425 MB/s
  2.3s/6000000 lines (⌀0.38µs), ~2623487 lines/s
  2.3s/1908 MB, ~834 MB/s
  0 CorrelIds generated, 0× string, 0× bindCorrelId, 0× CorrelId.current
  1 (0%) Log4jStringMap, 0× forEach, 0× getCopy, 0× get, 0× put (suppressed)
*/

final class Log4jTest extends OurTestSuite:

  private val testSpeed = sys.props contains "test.speed"
  private given IORuntime = ioRuntime

  "Speed" in:
    if !testSpeed then
      doTestSpeed(3, 3)
    else
      doTestSpeed(1000, 1000)
      sleep(500.ms)
      doTestSpeed(1000, 6000)
      logger.info(CorrelId.statistics)
      logger.info(Log4jThreadContextMap.statistics)

  private def doTestSpeed(n: Int, m: Int): Unit =
    val started = now
    (1 to n)
      .toVector
      .traverse: i =>
        CorrelId.bindNew:
          IO:
            var j = 1
            while j <= m do
              logger.debug(s"$i-$j $Line")
              j += 1
      .await(99.s)

    def log(line: String) =
      logger.info(line)
      if testSpeed then info(line)

    val elapsed = started.elapsed
    log(itemsPerSecondString(elapsed, n * m, "lines"))
    log(bytesPerSecondString(elapsed, n.toLong * m * EstimatedByteCount))


object Log4jTest:
  private lazy val logger = Logger[this.type]
  private val Line = " long" * 50
  private val EstimatedByteCount =
    40 + this.getClass.scalaName.length + 6 + Line.getBytes(UTF_8).length
