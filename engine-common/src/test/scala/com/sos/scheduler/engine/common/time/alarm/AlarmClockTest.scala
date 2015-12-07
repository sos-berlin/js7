package com.sos.scheduler.engine.common.time.alarm

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.common.time.alarm.AlarmClockTest._
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
final class AlarmClockTest extends FreeSpec with ScalaFutures {

  "Thread timeout and warm-up" in {
    new ConcurrentLinkedQueue[String]().add("WARM-UP")
    autoClosing(new AlarmClock(10.ms, idleTimeout = Some(1.s))) { alarmClock ⇒
      alarmClock.delay(0.s, "test") {}
      assert(alarmClock.isRunning)
      assert(waitForCondition(2.s, 10.ms) { !alarmClock.isRunning })
    }
  }

  "AlarmClock" in {
    autoClosing(new AlarmClock(10.ms, idleTimeout = Some(1.s))) { alarmClock ⇒
      for (nr ← 1 to 2) {
        val results = new ConcurrentLinkedQueue[(String, Instant)]()
        val t = Instant.now()
        alarmClock.at(t + 0.ms, "test") { results.add("A" → Instant.now()) }
        alarmClock.at(t + 400.ms, "test") { results.add("C" → Instant.now()) }
        alarmClock.at(t + 200.ms, "test") { results.add("B" → Instant.now()) }
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
    autoClosing(new AlarmClock(10.ms, idleTimeout = Some(1.s))) { alarmClock ⇒
      for (nr ← 1 to 2) {
        val results = new ConcurrentLinkedQueue[(String, Instant)]()
        val t = Instant.now()
        val promise = Promise[Unit]()

        alarmClock.delay(200.ms, cancelWhenCompleted = promise.future, "test") { results.add("200" → Instant.now()) }
        alarmClock.at(t + 400.ms, "test") { results.add("400" → Instant.now()) }
        val cAlarm = alarmClock.at(t + 600.ms, "test") { results.add("600" → Instant.now()) }
        alarmClock.at(t + 999.ms, cancelWhenCompleted = Future.successful(()), "test") { results.add("B" → Instant.now()) }  // Immediately ignored
        assert(alarmClock.overview.count == 3)
        promise.success(())
        alarmClock.cancel(cAlarm)
        sleep(100.ms)
        assert(alarmClock.overview.count == 1)
        sleep(600.ms)
        withClue(s"Run $nr: ") {
          val r = results.toVector
          assert((r map { _._1 }) == Vector("400"))
          assert(r(0)._2 >= t + 400.ms && r(0)._2 <= t + 500.ms)
        }
      }
    }
  }

  "Alarm is a Future" in {
    autoClosing(new AlarmClock(10.ms, idleTimeout = Some(1.s))) { alarmClock ⇒
      val a = alarmClock.delay(0.s, "test") { 777 }
      whenReady(a) { o ⇒ assert( o == 777 )}
    }
  }

  "Brute force" in {
    autoClosing(new AlarmClock(2.ms, idleTimeout = Some(1.s))) { alarmClock ⇒
      for (nr ← 1 to 100) {
        val counter = new AtomicInteger
        val n = 1000
        val delays = for (i ← 1 to n) yield Random.nextInt(40).ms
        for (delay ← delays) alarmClock.delay(delay, "test") { counter.incrementAndGet() }
        val ok = waitForCondition(2.s, 10.ms) { counter.get == n }
        if (!ok) logger.error(s"$counter/$n $alarmClock")
        assert(ok)
      }
    }
  }
}

private object AlarmClockTest {
  private val logger = Logger(getClass)
}
