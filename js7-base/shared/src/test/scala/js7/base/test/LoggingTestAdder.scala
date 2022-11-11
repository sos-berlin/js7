package js7.base.test

import js7.base.log.{CorrelId, Logger}
import js7.base.log.LoggingEscapeCodes.{blue, bold, green, orange, resetColor}
import js7.base.test.LoggingFreeSpecStringWrapper.{StringWrapper, TaggedAs}
import js7.base.test.LoggingTestAdder.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.Tests.{isIntelliJIdea, isSbt}
import org.scalatest.exceptions.TestPendingException
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration
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
      outerNames.toVector,
      testName)

  def afterAll(): Unit =
    logger.info(s"$suiteName — " +
      (if (succeededCount > 0) successMarkup else bold) +
      s"$succeededCount tests succeeded$resetColor" +
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

  final class TestContext(adder: LoggingTestAdder, val nesting: Seq[String], testName: String) {
    private lazy val since = now
    private val prefix = nesting.view.reverse.mkString("", " — ", " — ")

    def beforeTest(): Unit = {
      delayBeforeEnd()
      logger.info(eager(s"↘︎ $blue$bold$prefix$testName$resetColor"))
      delayBeforeEnd()
      since
    }

    def afterTest[A](tried: Try[A]): Unit = {
      val t = tried match {
        case Failure(t) =>
          clipStackTrace(t)
          Failure(t)
        case Success(_) => Success(())
      }
      val result = Result(prefix, testName, t, since.elapsed)
      val logLine = result.toLogLine
      tried match {
        case Success(_) =>
          adder.succeededCount += 1
          logger.info(s"↙︎ $logLine")
          logger.info(eager(successMarkup + bar))

        case Failure(_: TestPendingException) =>
          adder.pendingCount += 1
          logger.warn(logLine)
          logger.info(eager(pendingMarkup + bar))

        case Failure(t) =>
          clipStackTrace(t)
          adder.failedCount += 1
          if (isSbt) System.err.println(logLine)
          logger.error(logLine, t.nullIfNoStackTrace)
          logger.info(eager(failureMarkup + bar))
          clipStackTrace(t)
      }
      if (isIntelliJIdea) TestResultCollector.add(result)
      delayBeforeEnd()
    }
  }

  private[test] final case class Result(
    prefix: String,
    testName: String,
    tried: Try[Unit],
    duration: FiniteDuration)
  {
    def toLogLine: String =
      tried match {
        case Success(_) =>
          s"$successMarkup$prefix$testName$resetColor ${duration.pretty}"

        case Failure(_: TestPendingException) =>
          s"🚫 $pendingMarkup$prefix$testName (PENDING)$resetColor ${duration.pretty}"

        case Failure(t) =>
          s"💥 $failureMarkup$prefix$testName 💥$resetColor ${duration.pretty}"
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
