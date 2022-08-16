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
import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.control.NonFatal

/**
 * Extends `AnyFreeSpec` with logging of test names and their outcomes.
 * The log lines are colored, so use `less` with `LESS=-R` to let the escape sequences
 * take effect on your terminal.
 **/
trait LoggingAnyFreeSpec extends AnyFreeSpec {
  self =>

  private val outerNames = new mutable.Stack[String]

  protected implicit def implicitToFreeSpecStringWrapper(testName: String)
    (implicit pos: source.Position)
  : OurFreeSpecStringWrapper =
    new OurFreeSpecStringWrapper(testName, pos)

  private def callConvertToFreeSpecStringWrapper(testName: String, pos: source.Position) =
    super.convertToFreeSpecStringWrapper(testName)(pos)

  protected final class OurFreeSpecStringWrapper(testName: String, pos: source.Position)
  {
    private val underlying: FreeSpecStringWrapper =
      callConvertToFreeSpecStringWrapper(testName, pos)

    def -(f: => Unit): Unit =
      underlying - {
        outerNames.push(testName)
        try f
        finally {
          outerNames.pop()
        }
      }

    def in(f: => Unit): Unit = {
      val outer = outerNames.toVector
      underlying in executeTest(outer, f)
    }

    //def ignore(f: => Unit): Unit =
    //  underlying ignore f

    //def is(f: => PendingStatement): Unit =
    //  underlying is f

    def taggedAs(firstTag: Tag, more: Tag*): TaggedAsResult =
      new TaggedAsResult(firstTag, more)

    private def executeTest(outer: Seq[String], f: => Unit): Unit = {
      import AnsiEscapeCodes.{black, bold, green, red, reset}
      val prefix = (self.getClass.simpleScalaName +: outer).mkString("", " — ", " — ")
      delayBeforeEnd()
      logger.info(eager(s"↘︎ $prefix$black$bold$testName$reset"))
      delayBeforeEnd()
      try {
        f
        val markup = green + bold
        logger.info(eager(s"↙︎ $prefix$markup$testName$reset"))
        logger.info(eager(markup + "⎯" * 80))
        delayBeforeEnd()
      } catch {
        case NonFatal(t: TestPendingException) =>
          val markup = red
          logger.error(eager(s"🚫 $prefix${markup}$testName (PENDING)$reset\n"))
          logger.info(eager(markup + "⎯" * 80))
          delayBeforeEnd()
          throw t

        case NonFatal(t) =>
          clipStackTrace(t)

          val markup = red + bold
          val s = s"💥 $prefix$markup$testName 💥$reset"
          logger.error(s, t)
          logger.info(eager(markup + "⎯" * 80))
          if (isSbt) System.err.println(s)
          delayBeforeEnd()

          throw t
      }
    }

    protected final class TaggedAsResult(tag: Tag, more: Seq[Tag]) {
      val tagged = underlying.taggedAs(tag, more: _*)

      def in(f: => Unit): Unit = {
        val outer = outerNames.toVector
        tagged in executeTest(outer, f)
      }

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

  private def clipStackTrace(t: Throwable): Unit = {
    val st = t.getStackTrace
    val dropTop = st.indexWhere(o => !o.getClassName.startsWith("org.scalatest."))
    val dropBottom = st.lastIndexWhere { o =>
      val c = o.getClassName
      !droppableStackTracePrefixes.exists(c startsWith _)
    } match {
      case -1 => st.length
      case i => i + 1
    }

    if (dropTop < dropBottom) {
      t.setStackTrace(st.slice(dropTop, dropBottom))
    }
  }

  private def delayBeforeEnd() =
    if (isIntelliJIdea) sleep(1.ms)

  /** Because ScalaLogging String interpolation may let the
   * IntellijJScala plugin intersperse a '\n'. */
  private def eager(s: String) = s
}
