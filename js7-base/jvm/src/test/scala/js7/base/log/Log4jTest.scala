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
import scala.concurrent.duration.Deadline.now

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
              logger.debug(s"$i-$j bla bla bla bla bla bla bla bla bla bla bla bla bla")
              j += 1
      .await(199.s)

    def log(line: String) =
      logger.info(line)
      if testSpeed then info(line)

    val elapsed = started.elapsed
    log(itemsPerSecondString(elapsed, n * m, "lines"))
    log(bytesPerSecondString(elapsed, n.toLong * m * EstimatedByteCount))


object Log4jTest:
  private lazy val logger = Logger[this.type]
  private val LongLine = " long" * 200
  private val EstimatedByteCount = 5 + LongLine.getBytes(UTF_8).length
