package js7.base.log

import cats.syntax.parallel.*
import js7.base.log.Log4jTest.*
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.{DurationRichInt, RichDeadline, sleep}
import js7.base.time.Stopwatch
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.duration.Deadline.now

final class Log4jTest extends OurTestSuite:
  private val testSpeed = sys.props contains "test.speed"

  "Speed" in:
    if !testSpeed then
      testSpeed(3, 3)
    else
      testSpeed(1000, 1000)
      sleep(1.s)
      testSpeed(6000, 6000)
      logger.info(CorrelId.statistics)
      logger.info(CorrelIdLog4jThreadContextMap.statistics)

  private def testSpeed(n: Int, m: Int): Unit =
    val started = now
    (1 to n)
      .toVector
      .parTraverse(i =>
        CorrelId.bindNew(Task {
          for j <- 1 to m do {
            logger.debug(s"$i-$j")
          }
        }))
      .await(199.s)
    logger.info(Stopwatch.itemsPerSecondString(started.elapsed, n * m, "lines"))
    if testSpeed then info(Stopwatch.itemsPerSecondString(started.elapsed, n * m, "lines"))


object Log4jTest:
  private val logger = Logger[this.type]
