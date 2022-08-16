package js7.base.test

import js7.base.test.LoggingFreeSpecStringWrapper.*
import org.scalatest.{PendingStatement, Tag}
import scala.language.reflectiveCalls

final class LoggingFreeSpecStringWrapper[R](
  name: String,
  underlying: StringWrapper[R, TaggedAs[R]],
  testAdder: LoggingTestAdder,
  executeTest: (LoggingTestAdder.TestContext, => R) => R)
{
  def -(addTests: => Unit): Unit =
    underlying - {
      testAdder.addTests(name, addTests)
    }

  def in(testBody: => R): Unit = {
    val ctx = testAdder.freezeContext(name)
    underlying in {
      executeTest(ctx, testBody)
    }
  }

  //def ignore(f: => Unit): Unit =
  //  underlying ignore f

  //def is(f: => PendingStatement): Unit =
  //  underlying is f

  def taggedAs(firstTag: Tag, more: Tag*): Tagged =
    new Tagged(firstTag, more)

  protected final class Tagged(tag: Tag, more: Seq[Tag]) {
    private val taggedAs = underlying.taggedAs(tag, more *)

    def in(testBody: => R): Unit = {
      val ctx = testAdder.freezeContext(name)
      taggedAs in {
        executeTest(ctx, testBody)
      }
    }

    def is(pending: => PendingStatement): Unit =
      taggedAs is pending

    def ignore(testBody: => R): Unit =
      taggedAs ignore testBody
  }
}

object LoggingFreeSpecStringWrapper {
  type StringWrapper[R, T] = {
    def -(addTests: => Unit): Unit
    def in(testBody: => R): Unit
    //def is(pending: => PendingStatement): Unit
    //def ignore(testBody: => R): Unit
    def taggedAs(tag: Tag, more: Tag*): T
  }

  type TaggedAs[R] = {
    def in(testBody: => R): Unit
    def is(pending: => PendingStatement): Unit
    def ignore(testBody: => R): Unit
  }
}
