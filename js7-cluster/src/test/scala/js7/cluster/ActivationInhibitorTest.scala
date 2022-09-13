package js7.cluster

import js7.base.problem.{Checked, Problem}
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.cluster.ActivationInhibitor.{Active, Inhibited, Initial, Passive, State}
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Success

/**
  * @author Joacim Zschimmer
  */
final class ActivationInhibitorTest extends AnyFreeSpec
{
  private implicit val scheduler = TestScheduler()

  "startActive, startPassive" - {
    lazy val inhibitor = new ActivationInhibitor

    "state is Initial" in {
      val a = inhibitor.state.runToFuture
      scheduler.tick()
      assert(a.value == Some(Success(Some(Initial))))
    }

    "startActive" in {
      val a = inhibitor.startActive.runToFuture
      scheduler.tick()
      assert(a.value == Some(Success(())))
    }

    "Following startActive is rejected" in {
      val a = inhibitor.startActive.runToFuture
      scheduler.tick()
      assert(a.value.get.isFailure)
    }

    "Following startPassive is rejected" in {
      val a = inhibitor.startPassive.runToFuture
      scheduler.tick()
      assert(a.value.get.isFailure)
    }

    "state is Active" in {
      val a = inhibitor.state.runToFuture
      scheduler.tick()
      assert(a.value == Some(Success(Some(Active))))
    }
  }

  "tryActivate" - {
    implicit lazy val inhibitor = new ActivationInhibitor
    lazy val activation = succeedingActivation(inhibitor, Right(true))

    "first" in {
      val a = activation.runToFuture
      scheduler.tick()
      assert(!a.isCompleted)
      assert(inhibitor.state.await(99.s) == None)

      scheduler.tick(1.s)
      assert(a.value == Some(Success(Right(true))))
      assert(inhibitor.state.await(99.s) == Some(Active))
    }

    "second activation is allowed" in {
      // That means, the current activation is acknowledged
      val a = activation.runToFuture
      scheduler.tick(1.s)
      assert(a.value == Some(Success(Right(true))))
      assert(inhibitor.state.await(99.s) == Some(Active))
    }
  }

  "tryActivate but body rejects with Right(false)" in {
    implicit lazy val inhibitor = new ActivationInhibitor
    lazy val activation = succeedingActivation(inhibitor, Right(false))

    val a = activation.runToFuture
    scheduler.tick()
    assert(!a.isCompleted)
    assert(inhibitor.state.await(99.s) == None)

    scheduler.tick(1.s)
    assert(a.value == Some(Success(Right(false))))
    assert(inhibitor.state.await(99.s) == Some(Passive))
  }

  "tryActivate but body returns Left(problem)" in {
    implicit lazy val inhibitor = new ActivationInhibitor
    lazy val activation = succeedingActivation(inhibitor, Left(Problem("PROBLEM")))

    val a = activation.runToFuture
    scheduler.tick()
    assert(!a.isCompleted)
    assert(inhibitor.state.await(99.s) == None)

    scheduler.tick(1.s)
    assert(a.value == Some(Success(Left(Problem("PROBLEM")))))
    assert(inhibitor.state.await(99.s) == Some(Passive))
  }

  "tryActivate with failed activation" - {
    implicit lazy val inhibitor = new ActivationInhibitor

    "first" in {
      val a = inhibitor.startPassive.runToFuture
      scheduler.tick()
      assert(a.value == Some(Success(())))

      val b = failingActivation(inhibitor).runToFuture
      scheduler.tick()
      assert(!b.isCompleted)
      assert(inhibitor.state.await(99.s) == None)

      scheduler.tick(1.s)
      assert(b.value.map(_.failed.get.toString) == Some("java.lang.RuntimeException: TEST"))
      assert(inhibitor.state.await(99.s) == Some(Passive))
    }

    "again with succeeding activation" in {
      val a = succeedingActivation(inhibitor, Right(true)).runToFuture
      scheduler.tick(1.s)
      assert(a.value == Some(Success(Right(true))))
      assert(inhibitor.state.await(99.s) == Some(Active))
    }
  }

  "inhibitActivation" - {
    implicit lazy val inhibitor = new ActivationInhibitor

    "inhibitActivation" in {
      val a = inhibitor.startPassive.runToFuture
      scheduler.tick()
      assert(a.value == Some(Success(())))

      val whenInhibited = inhibitor.inhibitActivation(2.s).runToFuture
      scheduler.tick()
      assert(whenInhibited.value == Some(Success(Right(true))))
      assert(state == Some(Inhibited(1)))

      val b = succeedingActivation(inhibitor, Right(true)).runToFuture
      scheduler.tick()
      assert(b.value == Some(Success(Right(false))))
    }

    "while inhibition is in effect" in {
      assert(state == Some(Inhibited(1)))
      scheduler.tick(2.s)
      assert(state == Some(Passive))
    }

    "after inhibition has timed out, activation starts" in {
      val a = succeedingActivation(inhibitor, Right(true)).runToFuture
      scheduler.tick(1.s)
      assert(a.value == Some(Success(Right(true))))
      assert(inhibitor.state.await(99.s) == Some(Active))
    }

    "inhibitActivation returns false if state is active" in {
      val whenInhibited = inhibitor.inhibitActivation(2.s).runToFuture
      scheduler.tick()
      assert(whenInhibited.value == Some(Success(Right(false))))
    }
  }

  "inhibitActivation waits when currently activating" in {
    implicit val inhibitor = new ActivationInhibitor

    val a = inhibitor.startPassive.runToFuture
    scheduler.tick()
    assert(a.value == Some(Success(())))

    val b = succeedingActivation(inhibitor, Right(true)).runToFuture
    scheduler.tick()
    val whenInhibited = inhibitor.inhibitActivation(2.s).runToFuture

    scheduler.tick()
    assert(!whenInhibited.isCompleted)

    scheduler.tick(1.s)
    assert(b.value == Some(Success(Right(true))))
    assert(whenInhibited.value == Some(Success(Right(false))))
    assert(state == Some(Active))

    scheduler.tick(2.s)
    assert(state == Some(Active))
  }

  "inhibit while inhibiting" in {
    implicit val inhibitor = new ActivationInhibitor

    val a = inhibitor.startPassive.runToFuture
    scheduler.tick()
    assert(a.value == Some(Success(())))

    val aInhibited = inhibitor.inhibitActivation(2.s).runToFuture
    scheduler.tick()
    assert(aInhibited.value == Some(Success(Right(true))))

    scheduler.tick(1.s)
    assert(state == Some(Inhibited(1)))
    val bInhibited = inhibitor.inhibitActivation(2.s).runToFuture
    assert(bInhibited.value == Some(Success(Right(true))))
    assert(state == Some(Inhibited(2)))

    scheduler.tick(1.s)
    assert(state == Some(Inhibited(1)))

    scheduler.tick(1.s)
    assert(state == Some(Passive))
  }

  private def succeedingActivation(inhibitor: ActivationInhibitor, bodyResult: Checked[Boolean])
  : Task[Checked[Boolean]] =
    inhibitor.tryToActivate(
      ifInhibited = Task.right(false),
      activate = Task.pure(bodyResult).delayExecution(1.s))

  private def failingActivation(inhibitor: ActivationInhibitor): Task[Checked[Boolean]] =
    inhibitor.tryToActivate(
      ifInhibited = Task.right(false),
      activate = Task.raiseError(new RuntimeException("TEST")).delayExecution(1.s))

  private def state(implicit inhibitor: ActivationInhibitor): Option[State] = {
    val a = inhibitor.state.runToFuture
    scheduler.tick()
    a.value.get.get
  }
}
