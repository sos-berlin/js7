package js7.base.utils

import cats.effect.{Deferred, IO, Outcome, Resource}
import js7.base.problem.Problem
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.Atomic.extensions.*

final class MutableAllocatedTest extends OurAsyncTestSuite:

  "MutableAllocated" in:
    val a = MutableAllocated[Int]
    var count = 0
    val resource = Resource.make(
      acquire = IO(count))(
      release = _ => IO { count += 1 })

    for
      checked <- a.checked
      _ <- IO(assert(checked == Left(Problem(
        "js7.base.utils.MutableAllocatedTest#a: MutableAllocated[Int] has not been allocated"))))

      i <- a.acquire(resource)
      _ <- IO(assert(i == 0 && count == 0))

      i <- a.acquire(resource)
      _ <- IO(assert(i == 1 && count == 1))

      checked <- a.checked
      _ <- IO(assert(checked == Right(1)))

      _ <- a.release
      _ <- IO(assert(count == 2))

      checked <- a.checked
      _ <- IO(assert(checked == Left(Problem(
        "js7.base.utils.MutableAllocatedTest#a: MutableAllocated[Int] has not been allocated"))))

      _ <- a.releaseFinally
      either <- a.acquire(resource).attempt
      _ <- IO:
        either match
          case Left(t: a.AcquisitionCanceledException) => succeed
          case unexpected => fail(s"Unexpected result from acquire: $unexpected")
    yield
      succeed

  "releaseFinally while acquiring" in:
    val a = MutableAllocated[Int]
    val acquiring = Deferred.unsafe[IO, Unit]
    val acquisitionCanceled = Atomic(false)
    val acquireCanceled = Atomic(false)
    for
      fiber <- a
        .acquire:
          Resource.eval:
            acquiring.complete(()) *>
              IO.never.onCancel(IO:
                assert(!acquireCanceled.get)
                acquisitionCanceled := true)
        .onCancel(IO:
          assert(acquisitionCanceled.get)
          acquireCanceled := true)
        .start
      _ <- acquiring.get
      _ <- a.releaseFinally
      outcome <- fiber.join
    yield
      assert(acquisitionCanceled.get() /*&& acquireCanceled.get*/)
      outcome match
        //case Outcome.Canceled() => succeed
        case Outcome.Errored(_: a.AcquisitionCanceledException) => succeed
        case _ => fail("AcquisitionCanceledException expected")
