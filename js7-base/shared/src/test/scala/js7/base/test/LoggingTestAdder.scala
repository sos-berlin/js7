package js7.base.test

import js7.base.log.AnsiEscapeCodes.{black, bold, green, red, reset}
import js7.base.log.Log4j
import org.apache.logging.log4j.LogManager
import js7.base.test.LoggingFreeSpecStringWrapper.{StringWrapper, TaggedAs}
import js7.base.test.LoggingTestAdder.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Tests.{isIntelliJIdea, isSbt}
import org.scalatest.exceptions.TestPendingException
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

private final class LoggingTestAdder(suiteName: String) {

  Log4j.initialize()

  private val outerNames = Seq(suiteName).to(mutable.Stack)

  def toStringWrapper[R](
    name: String,
    wrapper: StringWrapper[R, TaggedAs[R]],
    executeTest: (LoggingTestAdder.TestContext, => R) => R)
  : LoggingFreeSpecStringWrapper[R] =
    new LoggingFreeSpecStringWrapper(name, wrapper, this, executeTest)

  def addTests(name: String, addTests: => Unit): Unit = {
    outerNames.push(name)
    try addTests
    finally outerNames.pop()
  }

  def freezeContext(testName: String) =
    new TestContext(
      outerNames.mkString("", " — ", " — "),
      testName)
}

private object LoggingTestAdder {
  private val logger = LogManager.getLogger("TEST")

  private val droppableStackTracePrefixes = Set(
    "java.",
    "scala.",
    "sbt.",
    "org.scalatest.",
    "org.jetbrains.plugins.scala.",
    "js7.base.test.")

  final class TestContext(val prefix: String, testName: String) {
    def beforeTest(): Unit = {
      delayBeforeEnd()
      logger.info(eager(s"↘︎ $prefix$black$bold$testName$reset"))
      delayBeforeEnd()
    }

    def afterTest[A](result: Try[A]): Unit = {
      result match {
        case Success(_) =>
          val markup = green + bold
          logger.info(eager(s"↙︎ $prefix$markup$testName$reset"))
          logger.info(eager(markup + "⎯" * 80))
          delayBeforeEnd()

        case Failure(_: TestPendingException) =>
          val markup = red
          logger.error(eager(s"🚫 $prefix$markup$testName (PENDING)$reset\n"))
          logger.info(eager(markup + "⎯" * 80))
          delayBeforeEnd()

        case Failure(t) =>
          clipStackTrace(t)

          val markup = red + bold
          val s = s"💥 $prefix$markup$testName 💥$reset"
          logger.error(s, t)
          logger.info(eager(markup + "⎯" * 80))
          if (isSbt) System.err.println(s)
          delayBeforeEnd()
      }
    }
  }

  private def clipStackTrace(t: Throwable): Unit = {
    val st = t.getStackTrace
    val dropTop = st.indexWhere(o => !o.getClassName.startsWith("org.scalatest."))
    val dropBottom = st.lastIndexWhere { o =>
      val c = o.getClassName
      !droppableStackTracePrefixes.exists(c startsWith _)
    }
    t.setStackTrace(st.slice(dropTop, dropBottom - 1 max dropTop + 1))
  }

  private def delayBeforeEnd() =
    if (isIntelliJIdea) sleep(1.ms)

  /** Because ScalaLogging String interpolation may let the
   * IntellijJScala plugin intersperse a '\n'. */
  private def eager(s: String) = s
}
