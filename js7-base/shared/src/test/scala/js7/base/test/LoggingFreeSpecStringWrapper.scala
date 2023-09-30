package js7.base.test

import js7.base.test.LoggingFreeSpecStringWrapper.*
import org.scalatest.{PendingStatement, Tag}

final class LoggingFreeSpecStringWrapper[R, T](
  name: String,
  underlying: UnifiedStringWrapper[R, T],
  testAdder: LoggingTestAdder,
  executeTest: (LoggingTestAdder.TestContext, => R) => R):

  def -(addTests: => Unit): Unit =
    underlying - {
      testAdder.addTests(name, addTests)
    }

  infix def in(testBody: => R): Unit =
    val ctx = testAdder.freezeContext(name)
    underlying.in:
      executeTest(ctx, testBody)

  //infix def ignore(f: => Unit): Unit =
  //  underlying ignore f

  //infix def is(f: => PendingStatement): Unit =
  //  underlying is f

  infix def taggedAs(firstTag: Tag, more: Tag*): Tagged =
    new Tagged(firstTag, more)

  protected final class Tagged(tag: Tag, more: Seq[Tag]):
    private val taggedAs = underlying.taggedAs(tag, more*)

    infix def in(testBody: => R): Unit =
      val ctx = testAdder.freezeContext(name)
      taggedAs.in:
        executeTest(ctx, testBody)

    infix def is(pending: => PendingStatement): Unit =
      taggedAs.is(pending)

    infix def ignore(testBody: => R): Unit =
      taggedAs.ignore(testBody)

object LoggingFreeSpecStringWrapper:

  private[test] trait UnifiedStringWrapper[R, T]:
    def -(addTests: => Unit): Unit

    def in(testBody: => R): Unit

    //def is(pending: => PendingStatement): Unit
    //def ignore(testBody: => R): Unit
    def taggedAs(tag: Tag, more: Tag*): TaggedAs[R]

  private[test] trait TaggedAs[R]:
    def in(testBody: => R): Unit

    def is(pending: => PendingStatement): Unit

    def ignore(testBody: => R): Unit
