package js7.base.test

import js7.base.log.{AnsiEscapeCodes, Logger}
import js7.base.test.LoggingAnyFreeSpec._
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.Tests.{isIntelliJIdea, isSbt}
import org.scalactic.source
import org.scalatest.exceptions.TestPendingException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{PendingStatement, Tag}
import scala.language.implicitConversions
import scala.util.control.NonFatal

trait LoggingAnyFreeSpec extends AnyFreeSpec {
  self =>

  protected implicit def implicitToFreeSpecStringWrapper(s: String)
    (implicit pos: source.Position)
  : MyFreeSpecStringWrapper =
    new MyFreeSpecStringWrapper(s, pos)

  private def wrappedConvertToFreeSpecStringWrapper(s: String, pos: source.Position) =
    super.convertToFreeSpecStringWrapper(s)(pos)

  protected final class MyFreeSpecStringWrapper(testName: String, pos: source.Position)
  {
    private val underlying: FreeSpecStringWrapper = wrappedConvertToFreeSpecStringWrapper(testName, pos)

    def -(f: => Unit): Unit =
      underlying - f

    def in(f: => Unit): Unit =
      underlying in executeTest(f)

    def ignore(f: => Unit): Unit =
      underlying ignore f

    def is(f: => PendingStatement): Unit =
      underlying is f

    def taggedAs(firstTag: Tag, more: Tag*): TaggedAsResult =
      new TaggedAsResult(firstTag, more)

    private def executeTest(f: => Unit): Unit = {
      import AnsiEscapeCodes.{black, bold, green, red, reset}
      val msg = s"${self.getClass.simpleScalaName} · $testName"
      logger.info(eager(s"$black$bold↘︎ $msg$reset"))
      delayBeforeEnd()
      try {
        f
        logger.info(eager(s"$green$bold↙︎ $msg$reset\n"))
        delayBeforeEnd()
      } catch {
        case NonFatal(t: TestPendingException) =>
          logger.error(eager(s"\u001B[31m🚫$msg · PENDING$reset\n"))
          delayBeforeEnd()
          throw t

        case NonFatal(t) =>
          reduceStackTrace(t)

          val s = s"$red$bold💥︎$msg 💥$reset"
          logger.error(s, t)
          if (isSbt) System.err.println(s)
          delayBeforeEnd()

          throw t
      }
    }

    private def delayBeforeEnd() =
      if (isIntelliJIdea) sleep(3.ms)

    /** Because ScalaLogging String interpolation may let ScalaTest intersperse a '\n'. */
    private def eager(s: String) = s

    protected final class TaggedAsResult(tag: Tag, more: Seq[Tag]) {
      val tagged = underlying.taggedAs(tag, more: _*)

      def in(f: => Unit): Unit =
        tagged in executeTest(f)

      def is(f: => PendingStatement): Unit =
        tagged is f

      def ignore(f: => Unit): Unit =
        tagged ignore f
    }
  }
}

object LoggingAnyFreeSpec {
  private val logger = Logger("TEST")
  private val droppableStackTracePrefixes = Set(
    "java.",
    "scala.",
    "sbt.",
    "org.scalatest.",
    "org.jetbrains.plugins.scala.",
    "js7.base.test.")

  private def reduceStackTrace(t: Throwable): Unit = {
    val st = t.getStackTrace
    var dropEnd = st.lastIndexWhere { o =>
      val c = o.getClassName
      !droppableStackTracePrefixes.exists(c startsWith _)
    }
    dropEnd = if (dropEnd == -1) st.length else dropEnd + 1

    val dropStart = st.indexWhere(o => !o.getClassName.startsWith("org.scalatest."))

    if (dropStart < dropEnd) {
      t.setStackTrace(st.slice(dropStart, dropEnd))
    }
  }
}
