package com.sos.scheduler.engine.common.time.timer

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.common.time.timer.TimerService._
import com.sos.scheduler.engine.common.time.timer.TimerServiceTest._
import java.time.{Duration, Instant}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.junit.JUnitRunner
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Random, Success}

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class TimerServiceTest extends FreeSpec with ScalaFutures {

  "Thread timeout and warm-up" in {
    new ConcurrentLinkedQueue[String]().add("WARM-UP")
    autoClosing(new TimerService(10.ms, idleTimeout = Some(1.s))) { timerService ⇒
      timerService.delay(0.s, "test")
      assert(timerService.isRunning)
      assert(waitForCondition(2.s, 10.ms) { !timerService.isRunning })
    }
  }

  "TimerService" in {
    autoClosing(new TimerService(10.ms, idleTimeout = Some(1.s))) { timerService ⇒
      for (nr ← 1 to 2) {
        val results = new ConcurrentLinkedQueue[(String, Instant)]()
        val t = Instant.now()
        timerService.at(t + 0.ms, "test") onElapsed { results.add("A" → Instant.now()) }
        timerService.at(t + 400.ms, "test") onElapsed { results.add("C" → Instant.now()) }
        timerService.at(t + 200.ms, "test") onElapsed { results.add("B" → Instant.now()) }
        sleep(500.ms)
        withClue(s"Run $nr: ") {
          val r = results.toVector
          logger.info(r map { case (s, i) ⇒ (s, i - t) } mkString " ")
          assert((r map { _._1 }) == Vector("A", "B", "C"))
          assert(r(0)._2 >= t && r(0)._2 <= t + 100.ms)
          assert(r(1)._2 >= t && r(1)._2 <= t + 300.ms)
          assert(r(2)._2 >= t && r(2)._2 <= t + 500.ms)
        }
      }
    }
  }

  "cancel, cancelWhenCompleted" in {
    autoClosing(new TimerService(10.ms, idleTimeout = Some(1.s))) { timerService ⇒
      for (nr ← 1 to 2) {
        val results = new ConcurrentLinkedQueue[(String, Instant)]()
        val t = Instant.now()
        val promise = Promise[Unit]()

        timerService.delay(200.ms, "test", cancelWhenCompleted = promise.future) onElapsed { results.add("200" → Instant.now()) }
        timerService.at(t + 400.ms, "test") onElapsed { results.add("400" → Instant.now()) }
        val cTimer = timerService.at(t + 600.ms, "test")
        cTimer onSuccess { case _ ⇒ results.add("600" → Instant.now()) }
        timerService.at(t + 999.ms, "test", cancelWhenCompleted = Future.successful(())) onElapsed { results.add("B" → Instant.now()) }  // Immediately ignored
        promise.success(())
        assert(!cTimer.canceled)
        timerService.cancel(cTimer)
        assert(cTimer.canceled)
        sleep(700.ms)
        withClue(s"Run $nr: ") {
          val r = results.toVector
          assert((r map { _._1 }) == Vector("400"))
          assert(r(0)._2 >= t + 400.ms && r(0)._2 <= t + 500.ms)
        }
        timerService.cancel(cTimer)
      }
    }
  }

  "onElapsed" in {
    autoClosing(new TimerService(10.ms, idleTimeout = Some(1.s))) { timerService ⇒
      val timer = timerService.delay(0.s, "test")
      val myPromise = Promise[Int]()
      val future: Future[Unit] = timer onElapsed { myPromise success 777 }
      assert(future eq timer)
      whenReady(myPromise.future) { o ⇒ assert(o == 777) }
      assert(timer.isCompleted)
      assert(timer.value == Some(Timer.ElapsedFailure))
      assert(myPromise.isCompleted)
      assert(myPromise.future.value == Some(Success(777)))
    }
  }

  "Timer is a Future, ElapsedException" in {
    autoClosing(new TimerService(10.ms, idleTimeout = Some(1.s))) { timerService ⇒
      val timer = timerService.delay(0.s, "test")
      val recovered = timer recover {
        case _: Timer.ElapsedException ⇒ 777
      }
      whenReady(recovered) { o ⇒ 777 }
      assert(timer.value == Some(Timer.ElapsedFailure))
    }
  }

  "completeWith" in {
    autoClosing(new TimerService(10.ms, idleTimeout = Some(1.s))) { timerService ⇒
      val a = timerService.delay(100.ms, "test", completeWith = Success(777))
      sleep(10.ms)
      assert(!a.isCompleted)
      whenReady(a) { o ⇒ assert(o == 777) }
    }
  }

  "fullfilWith and own promise" in {
    autoClosing(new TimerService(10.ms, idleTimeout = Some(1.s))) { timerService ⇒
      val promise = Promise[Int]()
      val a = timerService.delay(100.ms, "test", Success(777), promise)
      sleep(10.ms)
      assert(!promise.isCompleted && !a.isCompleted)
      whenReady(a) { o ⇒ assert(o == 777) }
      assert(promise.isCompleted && a.isCompleted)
      assert(promise.future.value == Some(Success(777)))
    }
  }

  "Brute force" in {
    autoClosing(new TimerService(2.ms, idleTimeout = Some(1.s))) { timerService ⇒
      for (nr ← 1 to 100) {
        val counter = new AtomicInteger
        val n = 1000
        val delays = for (i ← 1 to n) yield Random.nextInt(40).ms
        for (delay ← delays) timerService.delay(delay, "test") onElapsed { counter.incrementAndGet() }
        val ok = waitForCondition(2.s, 10.ms) { counter.get == n }
        if (!ok) logger.error(s"$counter/$n $timerService")
        assert(ok)
      }
    }
  }

  "Future.timeoutAfter" in {
    autoClosing(new TimerService(2.ms, idleTimeout = Some(1.s))) { implicit timerService ⇒
      def newFuture(a: Duration, timeout: Duration) = Future { sleep(a); "OK" } timeoutAfter (timeout, "test")
      whenReady(newFuture(50.ms, 100.ms)) {
        o ⇒ assert(o == "OK")
      }
      intercept[Timer.ElapsedException] {
        Await.result(newFuture(100.ms, 50.ms), 200.millis)
      }

      def newRecoveredFuture(a: Duration, timeout: Duration) = newFuture(a, timeout) recover { case _: Timer.ElapsedException ⇒ "TIMEOUT" }
      whenReady(newRecoveredFuture(50.ms, 100.ms)) { o ⇒ assert(o == "OK") }
      whenReady(newRecoveredFuture(100.ms, 50.ms)) { o ⇒ assert(o == "TIMEOUT") }
    }
  }
}

private object TimerServiceTest {
  private val logger = Logger(getClass)
}
