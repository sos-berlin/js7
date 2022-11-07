package js7.base.test

import js7.base.log.{CorrelId, Logger}
import js7.base.log.LoggingEscapeCodes.{blue, bold, green, orange, resetColor}
import js7.base.test.LoggingFreeSpecStringWrapper.{StringWrapper, TaggedAs}
import js7.base.test.LoggingTestAdder.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Tests.{isIntelliJIdea, isSbt}
import org.scalatest.exceptions.TestPendingException
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now
import scala.util.{Failure, Success, Try}

private final class LoggingTestAdder(suiteName: String) {

  Logger.initialize()

  private val since = now
  private val outerNames = Seq(suiteName).to(mutable.Stack)
  private var firstTestCalled = false
  private var succeededCount = 0
  private var pendingCount = 0
  private var failedCount = 0

  def toStringWrapper[R](
    name: String,
    wrapper: StringWrapper[R, TaggedAs[R]],
    executeTest: (LoggingTestAdder.TestContext, => R) => R,
    suppressCorrelId: Boolean)
  : LoggingFreeSpecStringWrapper[R] =
    new LoggingFreeSpecStringWrapper(name, wrapper, this,
      (ctx, test) => {
        if (!firstTestCalled) {
          firstTestCalled = true
          logger.info(bar)
        }
        if (suppressCorrelId)
          executeTest(ctx, test)
        else
          CorrelId.bindNow {
            executeTest(ctx, test)
          }
      })

  def addTests(name: String, addTests: => Unit): Unit = {
    outerNames.push(name)
    try addTests
    finally outerNames.pop()
  }

  def freezeContext(testName: String) =
    new TestContext(
      this,
      outerNames.view.reverse.mkString("", " — ", " — "),
      testName)

  def afterAll(): Unit =
    logger.info(s"$suiteName — " +
      (if (succeededCount > 0) successMarkup else bold) +
      s"$successMarkup$succeededCount tests succeeded$resetColor" +
      (if (failedCount == 0) "" else s" · $failureMarkup💥 $failedCount failed$resetColor") +
      (if (pendingCount == 0) "" else s" · $pendingMarkup🚫 $pendingCount pending$resetColor") +
      (if (failedCount == 0 && pendingCount == 0) s" $successMarkup✔︎$resetColor " else " · ") +
      since.elapsed.pretty + "\n")
}

private object LoggingTestAdder {
  private val logger = Logger("TEST")
  private val bar = "⎯" * 72
  private val successMarkup = green + bold
  private val pendingMarkup = ""
  private val failureMarkup = orange + bold

  private val droppableStackTracePrefixes = Set(
    "java.",
    "scala.",
    "sbt.",
    "org.scalatest.",
    "org.jetbrains.plugins.scala.",
    "js7.base.test.")

  final class TestContext(adder: LoggingTestAdder, val prefix: String, testName: String) {
    def beforeTest(): Unit = {
      delayBeforeEnd()
      logger.info(eager(s"↘︎ $blue$bold$prefix$testName$resetColor"))
      delayBeforeEnd()
    }

    def afterTest[A](result: Try[A]): Unit = {
      result match {
        case Success(_) =>
          adder.succeededCount += 1
          logger.info(eager(s"↙︎ $successMarkup$prefix$testName$resetColor"))
          logger.info(eager(successMarkup + bar))
          delayBeforeEnd()

        case Failure(_: TestPendingException) =>
          adder.pendingCount += 1
          logger.warn(eager(s"🚫 $pendingMarkup$prefix$testName (PENDING)$resetColor\n"))
          logger.info(eager(pendingMarkup + bar))
          delayBeforeEnd()

        case Failure(t) =>
          adder.failedCount += 1
          clipStackTrace(t)

          val s = s"💥 $failureMarkup$prefix$testName 💥$resetColor"
          logger.error(s, t)
          logger.info(eager(failureMarkup + bar))
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
    t.setStackTrace(st.slice(dropTop, dropBottom + 1 max dropTop + 1))
  }

  private def delayBeforeEnd() =
    if (false && isIntelliJIdea) sleep(1.ms)

  /** Because ScalaLogging String interpolation may let the
   * IntellijJScala plugin intersperse a '\n'. */
  private def eager(s: String) = s
}
