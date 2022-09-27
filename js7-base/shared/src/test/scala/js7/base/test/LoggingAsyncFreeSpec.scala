package js7.base.test

import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import org.scalactic.source
import org.scalatest.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.Try

/**
 * Extends `AnyFreeSpec` with logging of test names and their outcomes.
 * The log lines are colored, so use `less` with `LESS=-R` to let the escape sequences
 * take effect on your terminal.
 **/
trait LoggingAsyncFreeSpec extends AsyncFreeSpec {

  private val testAdder = new LoggingTestAdder(getClass.shortClassName)

  protected def suppressTestCorrelId = false

  protected implicit def implicitToFreeSpecStringWrapper(name: String)
    (implicit pos: source.Position)
  : LoggingFreeSpecStringWrapper[Future[Assertion]] =
    testAdder.toStringWrapper(
      name,
      super.convertToFreeSpecStringWrapper(name)(pos),
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
}
