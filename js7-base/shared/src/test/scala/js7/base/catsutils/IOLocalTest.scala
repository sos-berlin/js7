package js7.base.catsutils

import cats.effect.{IO, IOLocal}
import js7.base.test.OurAsyncTestSuite

/** Some tests to show how IOLocal works. */
final class IOLocalTest extends OurAsyncTestSuite:

  "Local" - {
    "monadic usage" in:
      for
        local <- IOLocal("")
        _ <- local.set("1")
        _ <- local.get.map(o => assert(o == "1"))
        _ <- IO.cede
        _ <- local.get.map(o => assert(o == "1"))
        _ <- local.set("?").start.flatMap(_.joinWithUnit)
        _ <- local.get.map(o => assert(o == "1"))
      yield succeed

    //"non-monadic use via jailbreak" in:
    //  import cats.effect.unsafe.IORuntime
    //  import js7.base.catsutils.UnsafeMemoizable.memoize
    //  import scala.annotation.unused
    //
    //  val local: IO[IOLocal[String]] = IOLocal("initial").memoize
    //  for
    //    _ <- local.flatMap(_.set("1"))
    //    _ <- local.flatMap(_.get).map(o => assert(o == "1"))
    //  yield
    //    @unused implicit def ioRuntime: IORuntime = throw new NotImplementedError
    //    val value = local.flatMap(_.get).syncStep(1000).unsafeRunSync().toOption.get
    //    assert(value == "1")
  }