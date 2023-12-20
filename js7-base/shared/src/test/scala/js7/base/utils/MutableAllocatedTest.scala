package js7.base.utils

import cats.effect.{IO, Resource}
import js7.base.problem.Problem
import js7.base.test.OurAsyncTestSuite

final class MutableAllocatedTest extends OurAsyncTestSuite:

  "MutableAllocated" in:
    val a = new MutableAllocated[Int]
    var count = 0
    val resource = Resource.make(
      acquire = IO(count))(
      release = _ => IO { count += 1 })

    val test = for
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
      _ <- a.finallyRelease
      _ <- IO(assert(checked == Left(Problem(
        "js7.base.utils.MutableAllocatedTest#a: MutableAllocated[Int] has not been allocated"))))
      either <- a.acquire(resource).attempt
      _ <- IO(assert(either.left.toOption.map(_.getMessage).contains(
        "js7.base.utils.MutableAllocatedTest#a: " +
          "MutableAllocated[Int]: has been finally released — new aqcuisition rejected")))
    yield succeed

    test.unsafeToFuture()
