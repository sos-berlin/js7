package js7.base.log

import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.{durationOf, measureTimeOfSingleRun}
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final class ScribeForJavaTest extends AnyFreeSpec
{
  ScribeForJava.coupleScribeWithSlf4j()

  private val scribeLogger = scribe.Logger(getClass.getName)
  private val logger = Logger(getClass)

  if (new Exception().getStackTrace.exists(_.getClassName == "org.jetbrains.plugins.scala.testingSupport.scalaTest.ScalaTestRunner")) {
    "Lazy arguments" in {
      if (logger.underlying.isTraceEnabled) {
        logger.warn("trace log level shoud be disabled for this test")
      } else {
        def f(duration: FiniteDuration) = {
          sleep(duration)
          duration.pretty
        }
        scribeLogger.debug(s"sleep ${f(0.s)}")
        assert(durationOf(scribeLogger.trace(s"sleep ${f(200.ms)}")) < 100.ms)
        val o = new AnyRef {
          override def toString = {
            sleep(200.ms)
            "200ms"
          }
        }
        assert(durationOf(scribeLogger.trace(s"sleep $o")) < 100.ms)
      }
    }

    val n = 100000
    val m = 5

    "Log4j speed test with arguments, trace" in {
      for (_ <- 1 to m) {
        scribeLogger.info(
          measureTimeOfSingleRun(n, "logger.trace") {
            for (i <- 1 to n) {
              logger.trace(s"TEST $i $i $i $i $i $i ")
            }
          }.toString)
      }
    }

    "Log4j speed test with arguments" in {
      for (_ <- 1 to m) {
        scribeLogger.info(
          measureTimeOfSingleRun(n, "logger.debug") {
            for (i <- 1 to n) {
              logger.debug(s"TEST $i $i $i $i $i $i ")
            }
          }.toString)
      }
    }

    "Log4j speed test without arguments" in {
      for (_ <- 1 to m) {
        scribeLogger.info(
          measureTimeOfSingleRun(n, "logger.debug") {
            for (_ <- 1 to n) {
              logger.debug("TEST")
            }
          }.toString)
      }
    }

    "Scribe speed test with arguments, trace" in {
      for (_ <- 1 to m) {
        scribeLogger.info(
          measureTimeOfSingleRun(n, "scribe.trace") {
            for (i <- 1 to n) {
              scribeLogger.trace(s"TEST $i $i $i $i $i $i ")
            }
          }.toString)
      }
    }

    "Scribe speed test with arguments" in {
      // No so fast if logger is disabled: (160ns/call when using scribe, 5ns/call when using ScalaLogger) !!!
      // When using dynamic scribe.trace: 700ns/call
      for (_ <- 1 to m) {
        scribeLogger.info(
          measureTimeOfSingleRun(n, "scribe.debug") {
            for (i <- 1 to n) {
              scribeLogger.debug(s"TEST $i $i $i $i $i $i ")
            }
          }.toString)
      }
    }

    "Scribe speed test without arguments" in {
      for (_ <- 1 to m) {
        scribeLogger.info(
          measureTimeOfSingleRun(n, "scribe.debug") {
            for (_ <- 1 to n) {
              scribeLogger.debug("TEST")
            }
          }.toString)
      }
    }
  }
}
