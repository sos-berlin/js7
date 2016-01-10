package com.sos.scheduler.engine.common.time.timer.signaling

import java.util.concurrent.CountDownLatch
import scala.concurrent.ExecutionContext.Implicits.global
import com.sos.scheduler.engine.common.time.ScalaTime._
import org.junit.runner.RunWith
import org.scalatest.Matchers._
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.duration.DurationInt
import scala.concurrent.{blocking, Await, Future}

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SignalingTest extends FreeSpec {

  for ((name, signaling) ← List(
    "SynchronizedSignaling" → new SynchronizedSignaling,
    "BlockingQueueSignaling" → new BlockingQueueSignaling,
    "BusyWaitingSignaling" → new BusyWaitingSignaling))
  name - {
    "Initially not signaled" in {
      assert(!signaling.isSignaled)
      signaling.awaitMillis(0) shouldEqual false
      signaling.awaitMillis(1) shouldEqual false
      assert(!signaling.isSignaled)
    }

    "awaitMillis(0) and isSignaled" in {
      signaling.signal()
      assert(signaling.isSignaled)
      signaling.awaitMillis(0) shouldEqual true
      assert(!signaling.isSignaled)
      signaling.awaitMillis(0) shouldEqual false
    }

    "awaitMillis(negative duration)" in {
      signaling.signal()
      assert(signaling.isSignaled)
      signaling.awaitMillis(-100) shouldEqual true
      assert(!signaling.isSignaled)
      signaling.awaitMillis(-100) shouldEqual false
    }

    "awaitMillis(positive duration)" in {
      signaling.signal()
      signaling.awaitMillis(1) shouldEqual true
      signaling.awaitMillis(1) shouldEqual false
      assert(!signaling.isSignaled)
    }

    "signal() is idempotent" in {
      signaling.signal()
      signaling.signal()
      signaling.signal()
      signaling.awaitMillis(1) shouldEqual true
      signaling.awaitMillis(1) shouldEqual false
    }

    "await" in {
      assert(!signaling.isSignaled)
      val latch = new CountDownLatch(2)
      val f = Future {
        assert(!signaling.isSignaled)
        blocking { latch.countDown() }
        signaling.await()
        signaling.isSignaled
      }
      latch.countDown()
      sleep(100.ms)
      signaling.signal()
      Await.result(f, 1.second) shouldEqual false
    }
  }
}
