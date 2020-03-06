package com.sos.jobscheduler.master.cluster

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.master.cluster.ActivationInhibitor.{Active, Inhibited, Passive, State}
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import org.scalatest.FreeSpec
import scala.util.Success

/**
  * @author Joacim Zschimmer
  */
final class ActivationInhibitorTest extends FreeSpec
{
  private implicit val scheduler = TestScheduler()

  "tryActivate" - {
    implicit lazy val inhibitor = new ActivationInhibitor
    lazy val activation = succeedingActivation(inhibitor)

    "first" in {
      val a = activation.runToFuture
      scheduler.tick()
      assert(!a.isCompleted)
      assert(inhibitor.state.await(99.s) == None)

      scheduler.tick(1.s)
      assert(a.value == Some(Success("ACTIVATED")))
      assert(inhibitor.state.await(99.s) == Some(Active))
    }

    "second activation is an error" in {
      val a = activation.runToFuture
      scheduler.tick()
      //Await.ready(a, 9.s)
      assert(a.value.map(_.failed.get.toString) ==
        Some("java.lang.IllegalStateException: ActivationInhibitor.tryToActivate but state is already Active"))
      assert(inhibitor.state.await(99.s) == None)  // Empty state after error !!!
    }
  }

  "tryActivate with failed activation" - {
    implicit lazy val inhibitor = new ActivationInhibitor

    "first" in {
      val a = failingActivation(inhibitor).runToFuture
      scheduler.tick()
      assert(!a.isCompleted)
      assert(inhibitor.state.await(99.s) == None)

      scheduler.tick(1.s)
      //Await.ready(a, 9.s)
      assert(a.value.map(_.failed.get.toString) == Some("java.lang.RuntimeException: TEST"))
      assert(inhibitor.state.await(99.s) == Some(Passive))
    }

    "again with succeeding activation" in {
      val a = succeedingActivation(inhibitor).runToFuture
      scheduler.tick(1.s)
      assert(a.value == Some(Success("ACTIVATED")))
      assert(inhibitor.state.await(99.s) == Some(Active))
    }
  }

  "inhibitActivation" - {
    implicit lazy val inhibitor = new ActivationInhibitor

    "inhibitActivation" in {
      val whenInhibited = inhibitor.inhibitActivation(2.s).runToFuture
      scheduler.tick()
      assert(whenInhibited.value == Some(Success(Right(true))))
      assert(state == Some(Inhibited))

      val a = succeedingActivation(inhibitor).runToFuture
      scheduler.tick()
      assert(a.value == Some(Success("INHIBITED")))
    }

    "while inhibition is in effect" in {
      assert(state == Some(Inhibited))
      scheduler.tick(2.s)
      assert(state == Some(Passive))
    }

    "after inhibition has timed out, activation starts" in {
      val a = succeedingActivation(inhibitor).runToFuture
      scheduler.tick(1.s)
      assert(a.value == Some(Success("ACTIVATED")))
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
    val a = succeedingActivation(inhibitor).runToFuture
    scheduler.tick()
    val whenInhibited = inhibitor.inhibitActivation(2.s).runToFuture

    scheduler.tick()
    assert(!whenInhibited.isCompleted)

    scheduler.tick(1.s)
    assert(a.value == Some(Success("ACTIVATED")))
    assert(whenInhibited.value == Some(Success(Right(false))))
    assert(state == Some(Active))

    scheduler.tick(2.s)
    assert(state == Some(Active))
  }

  "inhibit while inhibiting" in {
    implicit val inhibitor = new ActivationInhibitor
    val aInhibited = inhibitor.inhibitActivation(2.s).runToFuture
    scheduler.tick()
    assert(aInhibited.value == Some(Success(Right(true))))

    scheduler.tick(1.s)
    assert(state == Some(Inhibited))
    val bInhibited = inhibitor.inhibitActivation(2.s).runToFuture
    assert(bInhibited.value == Some(Success(Right(true))))
    assert(state == Some(Inhibited))

    // Second inhibitActivation renews inhibation timeout
    scheduler.tick(1.s)
    assert(state == Some(Inhibited))

    scheduler.tick(1.s)
    assert(state == Some(Passive))
  }

  private def succeedingActivation(inhibitor: ActivationInhibitor): Task[String] =
    inhibitor.tryToActivate(
      ifInhibited = Task.pure("INHIBITED"),
      activate = Task.pure("ACTIVATED").delayExecution(1.s))

  private def failingActivation(inhibitor: ActivationInhibitor): Task[String] =
    inhibitor.tryToActivate(
      ifInhibited = Task.pure("INHIBITED"),
      activate = Task.raiseError(new RuntimeException("TEST")).delayExecution(1.s))

  private def state(implicit inhibitor: ActivationInhibitor): Option[State] = {
    val a = inhibitor.state.runToFuture
    scheduler.tick()
    a.value.get.get
  }
}
