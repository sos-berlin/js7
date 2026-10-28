package js7.base.utils

import cats.effect.testkit.TestControl
import cats.effect.{Deferred, IO, Outcome, Resource}
import js7.base.time.ScalaTime.*
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

    TestControl.executeEmbed:
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
    TestControl.executeEmbed:
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

  "releaseFinally releases the resource" in:
    val a = MutableAllocated[Int]
    val releaseCount = Atomic(0)
    TestControl.executeEmbed:
      for
        _ <- a.acquire(Resource.make(IO(7))(_ => IO(releaseCount += 1)))
        _ <- a.releaseFinally
        _ <- IO(assert(releaseCount.get == 1))
        _ <- a.release
        _ <- a.releaseFinally
        checked <- a.checked
      yield
        assert(releaseCount.get == 1 && checked.isLeft)

  "releaseFinally while an uncancelable acquisition is completing, resource is released" in:
    val a = MutableAllocated[Int]
    val acquiring = Deferred.unsafe[IO, Unit]
    val continue = Deferred.unsafe[IO, Unit]
    val released = Atomic(false)
    TestControl.executeEmbed:
      for
        fiber <- a
          .acquire:
            Resource.make(
              acquire = acquiring.complete(()) *> continue.get.as(7))( // Uncancelable
              release = _ => IO(released := true))
          .start
        _ <- acquiring.get
        releaseFinallyFiber <- a.releaseFinally.start
        _ <- IO.sleep(10.ms) 
        _ <- continue.complete(())
        _ <- releaseFinallyFiber.joinWithUnit
        outcome <- fiber.join
        checked <- a.checked
      yield
        assert(released.get && checked.isLeft)
        outcome match
          case Outcome.Errored(_: a.AcquisitionCanceledException) => succeed
          case o => fail(s"AcquisitionCanceledException expected, but got $o")

  "acquire waiting for a running acquire is rejected after releaseFinally" in:
    val a = MutableAllocated[Int]
    val acquiring = Deferred.unsafe[IO, Unit]
    val secondAllocated = Atomic(false)
    TestControl.executeEmbed:
      for
        first <- a.acquire(Resource.eval(acquiring.complete(()) *> IO.never)).start
        _ <- acquiring.get
        second <- a.acquire(Resource.eval(IO(secondAllocated := true).as(2))).start
        _ <- IO.sleep(10.ms) // Let second wait for the lock
        _ <- a.releaseFinally
        firstOutcome <- first.join
        secondOutcome <- second.join
      yield
        assert(!secondAllocated.get)
        (firstOutcome, secondOutcome) match
          case (Outcome.Errored(_: a.AcquisitionCanceledException),
                Outcome.Errored(_: a.AcquisitionCanceledException)) => succeed
          case o => fail(s"AcquisitionCanceledException expected, but got $o")

  "Failed acquisition does not release the previous resource twice" in:
    val a = MutableAllocated[Int]
    val releaseCount = Atomic(0)
    TestControl.executeEmbed:
      for
        _ <- a.acquire(Resource.make(IO(1))(_ => IO(releaseCount += 1)))
        either <- a.acquire(Resource.eval(IO.raiseError[Int](new IllegalStateException("TEST")))).attempt
        _ <- IO(assert(either.left.exists(_.getMessage == "TEST")))
        checked <- a.checked
        _ <- IO(assert(checked.isLeft))
        _ <- a.release
        _ <- a.releaseFinally
      yield
        assert(releaseCount.get == 1)

  "Canceled acquire leaves MutableAllocated empty and usable" in:
    val a = MutableAllocated[Int]
    val acquiring = Deferred.unsafe[IO, Unit]
    val releaseCount = Atomic(0)
    TestControl.executeEmbed:
      for
        _ <- a.acquire(Resource.make(IO(1))(_ => IO(releaseCount += 1)))
        fiber <- a.acquire(Resource.eval(acquiring.complete(()) *> IO.never)).start
        _ <- acquiring.get
        _ <- fiber.cancel
        checked <- a.checked
        _ <- IO(assert(checked.isLeft && releaseCount.get == 1))
        i <- a.acquire(Resource.pure(3))
        _ <- a.releaseFinally
      yield
        assert(i == 3 && releaseCount.get == 1)
