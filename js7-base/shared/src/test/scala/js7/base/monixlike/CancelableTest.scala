package js7.base.monixlike

import cats.effect.IO
import js7.base.test.{OurTestSuite, TestCatsEffect}
import js7.base.thread.Futures.implicits.*
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future

final class CancelableTest extends OurTestSuite, TestCatsEffect:

  "unsafeCancelAndForget" in:
    pending
    //val cancelable: Cancelable = Cancelable:
    //  IO.never.onCancel(IO("canceled")).unsafeRunCancelable()
    //assert:
    //  cancelable.unsafeCancelAndForget().await(99.s) == "canceled"
