package js7.base.test

import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import org.scalatest.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.Try
import org.scalatest.{PendingStatement, Tag}

/**
 * Extends `AnyFreeSpec` with logging of test names and their outcomes.
 * The log lines are colored, so use `less` with `LESS=-R` to let the escape sequences
 * take effect on your terminal.
 **/
trait LoggingAsyncFreeSpec extends AsyncFreeSpec {

  private val testAdder = new LoggingTestAdder(getClass.shortClassName)

  protected def suppressTestCorrelId = false

  protected implicit inline def implicitToFreeSpecStringWrapper(name: String)
  : LoggingFreeSpecStringWrapper[Future[Assertion], ResultOfTaggedAsInvocationOnString] =
    testAdder.toStringWrapper(
      name,
      toUnified(/*super.convertToFreeSpecStringWrapper*/(name)),
      (ctx, testBody) =>
        Future(ctx.beforeTest())
          .flatMap(_ =>
            catchInFuture(testBody))
          .andThen { case tried =>
            ctx.afterTest(tried)
          },
      suppressCorrelId = suppressTestCorrelId)

  private def catchInFuture[A](startFuture: => Future[A]): Future[A] =
    Future.fromTry(Try(startFuture)).flatten

  private def toUnified(stringWrapper: FreeSpecStringWrapper) =
    new LoggingFreeSpecStringWrapper.UnifiedStringWrapper[Future[Assertion], ResultOfTaggedAsInvocationOnString] {
      def -(addTests: => Unit) =
        stringWrapper - addTests

      def in(testBody: => Future[Assertion]) =
        testBody

      def taggedAs(tag: Tag, more: Tag*) =
        new LoggingFreeSpecStringWrapper.TaggedAs[Future[Assertion]] {
          def in(testBody: => Future[Assertion]) =
            testBody

          def ignore(testBody: => Future[Assertion]) =
            testBody

          def is(pending: => PendingStatement) =
            pending
        }
    }
}
