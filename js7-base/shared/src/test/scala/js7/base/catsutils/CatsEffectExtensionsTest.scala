package js7.base.catsutils

import cats.effect
import cats.effect.Resource.ExitCase
import cats.effect.testkit.TestControl
import cats.effect.{Deferred, FiberIO, IO, Outcome, OutcomeIO, Resource, SyncIO}
import cats.syntax.option.*
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.CatsEffectUtils.FiberCanceledException
import js7.base.problem.Problem
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import scala.collection.mutable

final class CatsEffectExtensionsTest extends OurAsyncTestSuite:

  "IO" - {
    "guaranteeCaseLazy" - {
      "ERROR: guaranteeCase evaluates the Canceled case eagerly even when not Canceled" in:
        val evaluatedCases = mutable.Buffer.empty[OutcomeIO[Int]]
        val ioCases = mutable.Buffer.empty[OutcomeIO[Int]]

        IO(7)
          .guaranteeCase: outcome =>
            evaluatedCases += outcome
            IO { ioCases += outcome }
          .map: _ =>
            assert:
              evaluatedCases.toString == "ArrayBuffer(Canceled(), Succeeded(IO(7)))" &&
              ioCases.toString == "ArrayBuffer(Succeeded(IO(7)))"

      "guaranteeCaseLazy evalutes the Canceled case only when Canceled" in:
        val outcomes = mutable.Buffer.empty[OutcomeIO[Int]]

        IO(7)
          .guaranteeCaseLazy: outcome =>
            outcomes += outcome
            IO.unit
          .map: _ =>
            assert(outcomes.toString == "ArrayBuffer(Succeeded(IO(7)))")
    }

    "raceBoth" in:
      IO.raceBoth(
          IO.sleep(1.s).as(1),
          IO.sleep(2.s).as("2"))
        .flatMap:
          case Left((left: Int, fiber: FiberIO[String])) => fiber.cancel.as(left)
          case Right((fiber: FiberIO[Int]), right: String) => fiber.cancel.as(right)
        .map: value =>
          assert(value == 1)

    "onCancelLazy" - {
      "ERROR: onCancel evaluates the handler even when not canceled" in :
        var canceledEvaluated = false

        IO(7)
          .onCancel:
            canceledEvaluated = true
            IO.unit
          .map: _ =>
            assert(canceledEvaluated)

      "onCancelLazy evaluate the handler only when canceled" in :
        var canceledEvaluated = false

        IO(7)
          .onCancelLazy:
            canceledEvaluated = true
            IO.unit
          .map: _ =>
            assert(!canceledEvaluated)
    }

    "cancelWhen" in:
      TestControl.executeEmbed:
        for
          notCanceled <- IO(7).cancelWhen(IO.never)
          trigger <- Deferred[IO, Unit]
          io = IO(7).delayBy(2.s).cancelWhen(trigger.get)
          fiber <- IO.both(trigger.complete(()).delayBy(1.s), io.start).map(_._2)
          canceledOutcome <- fiber.join
        yield
          assert(notCanceled == 7 && canceledOutcome == Outcome.Canceled())

    //"onCancellation" - {
    //  "success" in:
    //    val canceled = Atomic(false)
    //    IO(7)
    //      .onCancellation:
    //        IO(canceled := true)
    //      .map: result =>
    //        assert(result == 7 && !canceled.get)
    //
    //  "failure" in:
    //    val canceled = Atomic(false)
    //    IO.raiseError[Int](new RuntimeException("TEST"))
    //      .onCancellation:
    //        IO(canceled := true)
    //      .attempt
    //      .map:
    //        case Left(e: RuntimeException) => assert(e.getMessage == "TEST" && !canceled.get)
    //        case o => fail(s"Exception expected, not: $o")
    //
    //  "cancel" in repeatTest(10_000): _ =>
    //    // onCancellation is not in effect if cancellation occurs before .onCancellation is executed !!!
    //    val canceled, cancellation = Atomic(false)
    //    val started = Deferred.unsafe[IO, Unit]
    //    started.complete(())
    //      .*>(IO.never)
    //      .onCancel(IO:
    //        assert(!cancellation.get)
    //        canceled := true)
    //      .onCancellation(IO:
    //        assert(canceled.get)
    //        cancellation := true)
    //      .start
    //      .flatMap: fiber =>
    //        /*IO.sleep(10.ms) // Wait until fiber executes IO.never
    //          *>*/ started.get
    //          *> fiber.cancel
    //          *> fiber.join
    //      .map: outcome =>
    //        assert(outcome == Outcome.Canceled() & canceled.get & cancellation.get)
    //}
  }

  "IO[Checked[x]]" - {
    "recoverFromProblemAndRetry" in:
      IO.left(Problem("PROBLEM"))
        .recoverFromProblemAndRetry(1): (problem, i, retry) =>
          if i < 10_000_000 then
            retry(i + 1)
          else
            IO.right(7)
        .map: result =>
          assert(result == Right(7))

    "recoverFromProblemAndRetry returning Left" in:
      IO.left(Problem("PROBLEM"))
        .recoverFromProblemAndRetry(1): (problem, i, retry) =>
          IO.left(Problem("OTHER PROBLEM"))
        .map: checked =>
          assert(checked == Left(Problem("OTHER PROBLEM")))
  }

  "Fiber" - {
    "joinStd throws when Fiber has been canceled" in:
      val exception = new Exception("TEST")
      for
        fiber <- IO(7).start
        joined <- fiber.joinStd
        _ = assert(joined == 7)

        fiber <- IO.raiseError(exception).start
        joined <- fiber.joinStd.attempt
        _ = assert(joined == Left(exception))

        fiber <- IO.never.start
        _ <- fiber.cancel
        joined <- fiber.joinStd.attempt
      yield
        joined match
          case Left(t: FiberCanceledException) => assert(t.toString ==
            "js7.base.catsutils.CatsEffectUtils$FiberCanceledException: Fiber has been canceled")
          case _ => fail("Unexpected result vom .joinStd")
  }

  "Resource" - {
    "orIfNone" in:
      var aAcquired, bAcquired, aReleased, bReleased = 0

      def aResource(a: Option[Int]) =
        Resource.make(SyncIO { aAcquired += 1; a })(_ => SyncIO { aReleased += 1 } )

      def bResource(b: Int) =
        Resource.make(SyncIO { bAcquired += 1; b })(_ => SyncIO { bReleased += 1 } )

      assert(aResource(1.some).orIfNone(bResource(2)).use(SyncIO(_)).unsafeRunSync() == 1)
      assert(aAcquired == 1 && aReleased == 1 && bAcquired == 0 && bReleased == 0)

      assert(aResource(none).orIfNone(bResource(2)).use(SyncIO(_)).unsafeRunSync() == 2)
      assert(aAcquired == 2 && aReleased == 2 && bAcquired == 1 && bReleased == 1)

    "makeCancelable" in:
      val canceled = Atomic(false)
      val resource = Resource.makeCancelable[IO, Unit](
        acquire = IO.never.onCancel(IO(canceled := true)))(
        release = _ => IO.unit)

      for
        fiber <- resource.surround(IO.unit).start
        _ <- IO.sleep(100.ms)
        _ <- fiber.cancel
        _ <- fiber.joinWithUnit
      yield
        assert(canceled.get)

    "makeCaseCancelable" in:
      val canceled = Atomic(false)
      val releaseExitCase = Atomic(none[ExitCase])
      val resource = Resource.makeCaseCancelable[IO, Unit](
        acquire = IO.never.onCancel(IO(canceled := true)))(
        release = (_, exitCase) => IO(releaseExitCase := exitCase.some))

      for
        fiber <- resource.surround(IO.unit).start
        _ <- IO.sleep(100.ms)
        _ <- fiber.cancel
        _ <- fiber.joinWithUnit
      yield
        assert(canceled.get && releaseExitCase.get == None)
  }
