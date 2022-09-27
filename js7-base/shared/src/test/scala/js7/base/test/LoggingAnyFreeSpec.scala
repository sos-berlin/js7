package js7.base.test

import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{Args, Status}
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

  protected implicit def implicitToFreeSpecStringWrapper(name: String)
    (implicit pos: source.Position)
  : LoggingFreeSpecStringWrapper[Any] =
    testAdder.toStringWrapper(
      name,
      super.convertToFreeSpecStringWrapper(name)(pos),
      (ctx, testBody) => {
        ctx.beforeTest()
        val tried = Try(testBody)
        ctx.afterTest(tried)
        for (t <- tried.failed) throw t
      },
      suppressCorrelId = suppressTestCorrelId)

  abstract override def run(testName: Option[String], args: Args): Status = {
    val tried = Try(super.run(testName, args))
    testAdder.afterAll()
    tried.get
  }
}
