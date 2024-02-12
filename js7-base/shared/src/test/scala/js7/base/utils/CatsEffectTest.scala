package js7.base.utils

import cats.effect.{IO, Resource}
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.*

final class CatsEffectTest extends OurAsyncTestSuite:

  "IO.never is cancelable" in:
    val canceled = Atomic(false)

    for
      fiber <- IO.never.as(()).onCancel(IO(canceled := true)).start
      _ <- IO.sleep(100.ms)
      _ <- fiber.cancel
      _ <- fiber.joinWithUnit
    yield
      assert(canceled.get)

  "Resource.make acquisition is not cancelable" ignore:
    // Test blocks forever
    val canceled = Atomic(false)
    val resource = Resource.make(
      acquire = IO.never.onCancel(IO(canceled := true)))(
      release = _ => IO.unit)

    for
      fiber <- resource.surround(IO.unit).start
      _ <- IO.sleep(100.ms)
      _ <- fiber.cancel.timeout(1.s)
      _ <- fiber.joinWithUnit.timeout(1.s)
    yield
      assert(!canceled.get)

  "Resource.makeFull acquisition is cancelable" in:
    val canceled = Atomic(false)
    val resource = Resource.makeFull[IO, Unit](
      acquire = poll => poll(IO.never.onCancel(IO(canceled := true))))(
      release = _ => IO.unit)

    for
      fiber <- resource.surround(IO.unit).start
      _ <- IO.sleep(100.ms)
      _ <- fiber.cancel
      _ <- fiber.joinWithUnit
    yield
      assert(canceled.get)
