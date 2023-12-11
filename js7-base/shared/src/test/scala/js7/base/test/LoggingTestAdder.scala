package js7.base.test

import js7.base.log.LoggingEscapeCodes.{bold, green, magenta, orange, resetColor}
import js7.base.log.{CorrelId, Logger}
import js7.base.test.LoggingFreeSpecStringWrapper.UnifiedStringWrapper
import js7.base.test.LoggingTestAdder.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.{RichString, RichThrowable}
import js7.base.utils.Tests.isSbt
import org.scalatest.exceptions.TestPendingException
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

private final class LoggingTestAdder(suiteName: String):

  logger.info(s"$magenta${"━" * barLength} $bold↘ $suiteName$resetColor")

  private lazy val since = now
  private val outerNames = Seq(suiteName).to(mutable.Stack)
  private var firstTestCalled = false
  private var succeededCount = 0
  private var pendingCount = 0
  private var failedCount = 0

  def toStringWrapper[R, T](
    name: String,
    wrapper: UnifiedStringWrapper[R, T],
    executeTest: (LoggingTestAdder.TestContext, => R) => R,
    suppressCorrelId: Boolean)
  : LoggingFreeSpecStringWrapper[R, T] =
    new LoggingFreeSpecStringWrapper(name, wrapper, this,
      (ctx, test) => {
        if !firstTestCalled then {
          firstTestCalled = true
          logger.info("\n" + magenta + "━" * barLength + resetColor)
        }
        if suppressCorrelId then
          executeTest(ctx, test)
        else
          CorrelId.bindNow {
            executeTest(ctx, test)
          }
      })

  def addTests(name: String, addTests: => Unit): Unit =
    outerNames.push(name)
    try addTests
    finally outerNames.pop()

  def freezeContext(testName: String): TestContext =
    since
    new TestContext(this, outerNames.toVector, testName)

  def afterAll(): Unit =
    logger.info(s"$suiteName — " +
      (if succeededCount == 0 then failureMarkup
      else if succeededCount > 0 then successMarkup
      else bold) +
      s"$succeededCount tests succeeded$resetColor" +
      (if failedCount == 0 then "" else s" · $failureMarkup💥 $failedCount failed$resetColor") +
      (if pendingCount == 0 then "" else s" · $pendingMarkup🚧 $pendingCount pending$resetColor") +
      (if failedCount == 0 && pendingCount == 0 then s" $successMarkup✔︎$resetColor " else " · ") +
      since.elapsed.pretty)
    logger.info(s"$magenta${"▲" * barLength} $bold↙ $suiteName$resetColor\n")


private object LoggingTestAdder:
  Logger.initialize("JS7 Test")  // In case it is not yet initialized

  private val logger = Logger("TEST")
  private val barLength = 100
  private val bar = "╼" * barLength
  private val successMarkup = green + bold
  private val pendingMarkup = ""
  private val failureMarkup = orange + bold

  private val collectResult: Result => Unit =
    TestResultCollector.add
  // Trying to use a sbt-wide TestResultCollector, not the instance for the respective
  // subproject ClassLoader created by ScalaTest. Fails with ClassNotFoundException.
  //  result => {
  //    val cls = ClassLoader.getSystemClassLoader
  //      .loadClass(classOf[TestResultCollector.type].getName)  ---> ClassNotFoundException
  //    val singletonField = cls.getDeclaredField("singleton")
  //    singletonField.setAccessible(true)
  //    val singleton = singletonField.get(null)
  //    cls.getMethod("add", classOf[Result]).invoke(singleton, result)
  //  }

  private val droppableStackTracePrefixes = Set(
    "java.",
    "scala.",
    "sbt.",
    "org.scalatest.",
    "org.jetbrains.plugins.scala.",
    "js7.base.test.")

  final class TestContext(adder: LoggingTestAdder, val nesting: Seq[String], testName: String):
    private lazy val since = now
    private val prefix = nesting.view.reverse.mkString("", " — ", " — ")

    def beforeTest(): Unit =
      logger.info(eager(s"↘ $magenta$bold$prefix$testName$resetColor"))
      since

    def afterTest[A](tried: Try[A]): Unit =
      val t = tried match
        case Failure(t) =>
          clipStackTrace(t)
          Failure(t)
        case Success(_) => Success(())
      val result = Result(prefix, testName, t, since.elapsed)
      val logLine = result.toLogLine
      tried match
        case Success(_) =>
          adder.succeededCount += 1
          logger.info(logLine)
          logger.info(eager(successMarkup + bar + resetColor))

        case Failure(_: TestPendingException) =>
          adder.pendingCount += 1
          logger.warn(logLine)
          logger.info(eager(pendingMarkup + bar + resetColor))

        case Failure(t) =>
          clipStackTrace(t)
          adder.failedCount += 1
          if isSbt then System.err.println(logLine)
          logger.error(logLine, t.nullIfNoStackTrace)
          logger.info(eager(failureMarkup + bar + resetColor))
          clipStackTrace(t)

      collectResult(result)

  private[test] final case class Result(
    prefix: String,
    testName: String,
    tried: Try[Unit],
    duration: FiniteDuration):
    val prettyDuration: String = duration.pretty

    def toLogLine: String =
      tried match
        case Success(_) =>
          s"$successMarkup↙ $prefix$testName$resetColor $prettyDuration"

        case Failure(_: TestPendingException) =>
          s"🚧 $pendingMarkup$prefix$testName (PENDING)$resetColor $prettyDuration"

        case Failure(t) =>
          s"💥 $failureMarkup$prefix$testName 💥$resetColor $prettyDuration"

    def toSummaryLine: String =
      val shortTestName = testName.truncateWithEllipsis(100) // Replaces \n
      tried match
        case Success(_) =>
          f"✔️  $prettyDuration%-7s $successMarkup$prefix$shortTestName$resetColor"

        case Failure(_: TestPendingException) =>
          f"🚧 $prettyDuration%-7s $pendingMarkup$prefix$shortTestName (PENDING)$resetColor"

        case Failure(t) =>
          f"💥 $prettyDuration%-7s $failureMarkup$prefix$shortTestName 💥$resetColor"

  private def clipStackTrace(t: Throwable): Unit =
    val st = t.getStackTrace
    val dropTop = st.indexWhere(o => !o.getClassName.startsWith("org.scalatest."))
    val dropBottom = st.lastIndexWhere { o =>
      val c = o.getClassName
      !droppableStackTracePrefixes.exists(c startsWith _)
    }
    t.setStackTrace(st.slice(dropTop, dropBottom + 1 max dropTop + 1))

  /** Because ScalaLogging String interpolation may let the
   * IntellijJScala plugin intersperse a '\n'. */
  private def eager(s: String) = s
