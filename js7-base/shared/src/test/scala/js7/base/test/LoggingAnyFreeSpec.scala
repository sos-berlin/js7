package js7.base.test

import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{Args, PendingStatement, Status, Tag}
import scala.language.implicitConversions
import scala.util.Try

/**
 * Extends `AnyFreeSpec` with logging of test names and their outcomes.
 * The log lines are colored, so use `less` with `LESS=-R` to let the escape sequences
 * take effect on your terminal.
 **/
trait LoggingAnyFreeSpec extends AnyFreeSpec {

  private val testAdder = new LoggingTestAdder(getClass.shortClassName)

  protected def suppressTestCorrelId = false

  protected implicit inline final def implicitToFreeSpecStringWrapper(name: String)
  : LoggingFreeSpecStringWrapper[Any, ResultOfTaggedAsInvocationOnString] =
    testAdder.toStringWrapper(
      name,
      toUnified(/*convertToFreeSpecStringWrapper*/(name)),
      (ctx, testBody) => {
        ctx.beforeTest()
        val tried = Try(testBody)
        ctx.afterTest(tried)
        for t <- tried.failed do throw t
      },
      suppressCorrelId = suppressTestCorrelId)

  abstract override def run(testName: Option[String], args: Args): Status = {
    val tried = Try(super.run(testName, args))
    testAdder.afterAll()
    tried.get
  }

  private def toUnified(stringWrapper: FreeSpecStringWrapper) =
    new LoggingFreeSpecStringWrapper.UnifiedStringWrapper[Any, ResultOfTaggedAsInvocationOnString] {
      def -(addTests: => Unit) =
        stringWrapper - addTests

      def in(testBody: => Any) =
        stringWrapper in testBody

      def taggedAs(tag: Tag, more: Tag*) =
        new LoggingFreeSpecStringWrapper.TaggedAs[Any] {
          def in(testBody: => Any) =
            testBody

          def ignore(testBody: => Any) =
            testBody

          def is(pending: => PendingStatement) =
            pending
        }
    }
}
