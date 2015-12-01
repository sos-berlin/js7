package com.sos.scheduler.engine.common.time.alarm

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.common.time.alarm.AlarmClockTest._
import java.time.Instant
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.FreeSpec
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class AlarmClockTest extends FreeSpec {

  "Thread timeout and warm-up" in {
    new ConcurrentLinkedQueue[String]().add("WARM-UP")
    autoClosing(new AlarmClock(10.ms, idleTimeout = Some(1.s))) { alarmClock ⇒
      alarmClock.delay(0.s) {}
      assert(alarmClock.isRunning)
      assert(waitForCondition(2.s, 10.ms) { !alarmClock.isRunning })
    }
  }

  "AlarmClock" in {
    autoClosing(new AlarmClock(2.ms, idleTimeout = Some(1.s))) { alarmClock ⇒
      for (nr ← 1 to 2) {
        val results = new ConcurrentLinkedQueue[(String, Instant)]()
        val t = Instant.now()
        alarmClock.at(t + 0.ms) { results.add("A" → Instant.now()) }
        alarmClock.at(t + 400.ms) { results.add("C" → Instant.now()) }
        alarmClock.at(t + 200.ms) { results.add("B" → Instant.now()) }
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

  "Brute force" in {
    autoClosing(new AlarmClock(2.ms, idleTimeout = Some(1.s))) { alarmClock ⇒
      for (nr ← 1 to 100) {
        val counter = new AtomicInteger
        val n = 1000
        val delays = for (i ← 1 to n) yield Random.nextInt(40).ms
        for (delay ← delays) alarmClock.delay(delay) { counter.incrementAndGet() }
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
