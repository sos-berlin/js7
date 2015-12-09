package com.sos.scheduler.engine.common.time.timer

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.common.time.timer.TimerServiceTest._
import java.time.Instant
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.junit.JUnitRunner
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Random

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
        timerService.at(t + 0.ms, "test") then_ { results.add("A" → Instant.now()) }
        timerService.at(t + 400.ms, "test") then_ { results.add("C" → Instant.now()) }
        timerService.at(t + 200.ms, "test") then_ { results.add("B" → Instant.now()) }
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

        timerService.delay(200.ms, "test").cancelWhenCompleted(promise.future) then_ { results.add("200" → Instant.now()) }
        timerService.at(t + 400.ms, "test") then_ { results.add("400" → Instant.now()) }
        val cTimer = timerService.at(t + 600.ms, "test")
        cTimer onSuccess { case _ ⇒ results.add("600" → Instant.now()) }
        timerService.at(t + 999.ms, "test").cancelWhenCompleted(Future.successful(())) then_ { results.add("B" → Instant.now()) }  // Immediately ignored
        assert(timerService.overview.count == 3)
        promise.success(())
        timerService.cancel(cTimer)
        sleep(100.ms)
        assert(timerService.overview.count == 1)
        sleep(600.ms)
        withClue(s"Run $nr: ") {
          val r = results.toVector
          assert((r map { _._1 }) == Vector("400"))
          assert(r(0)._2 >= t + 400.ms && r(0)._2 <= t + 500.ms)
        }
      }
    }
  }

  "Timer is a Future" in {
    autoClosing(new TimerService(10.ms, idleTimeout = Some(1.s))) { timerService ⇒
      val a = timerService.delay(0.s, "test")
      whenReady(a) { o ⇒ }
    }
  }

  "Brute force" in {
    autoClosing(new TimerService(2.ms, idleTimeout = Some(1.s))) { timerService ⇒
      for (nr ← 1 to 100) {
        val counter = new AtomicInteger
        val n = 1000
        val delays = for (i ← 1 to n) yield Random.nextInt(40).ms
        for (delay ← delays) timerService.delay(delay, "test") then_ { counter.incrementAndGet() }
        val ok = waitForCondition(2.s, 10.ms) { counter.get == n }
        if (!ok) logger.error(s"$counter/$n $timerService")
        assert(ok)
      }
    }
  }
}

private object TimerServiceTest {
  private val logger = Logger(getClass)
}
