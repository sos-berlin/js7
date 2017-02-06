package com.sos.scheduler.engine.common.time.timer

import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.Stopwatch
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.common.time.timer.TimerService._
import com.sos.scheduler.engine.common.time.timer.TimerServiceTest._
import java.lang.System.nanoTime
import java.time.Instant.now
import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ArrayBlockingQueue, ConcurrentLinkedQueue}
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.concurrent.ScalaFutures
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, Promise, blocking}
import scala.util.{Random, Success}

/**
  * @author Joacim Zschimmer
  */
final class TimerServiceTest extends FreeSpec with ScalaFutures {

  "Thread timeout and warm-up" in {
    new ConcurrentLinkedQueue[String]().add("WARM-UP")
    autoClosing(TimerService(idleTimeout = Some(1.s))) { timerService ⇒
      timerService.delayed(200.ms)
      assert(timerService.isRunning)
      assert(waitForCondition(2.s, 10.ms) { !timerService.isRunning })
    }
  }

  "TimerService" in {
    val timerService = TimerService(idleTimeout = Some(1.s))
    for (nr ← 1 to 3) {
      val results = new ConcurrentLinkedQueue[(String, Instant)]()
      val t = now
      timerService.at(t + 0.ms, "test") onElapsed { results.add("A" → now) }
      timerService.at(t + 400.ms, "test") onElapsed { results.add("C" → now) }
      timerService.at(t + 200.ms, "test") onElapsed { results.add("B" → now) }
      sleep(500.ms)
      withClue(s"Run $nr: ") {
        val r = results.toVector
        logger.info((r map { case (s, i) ⇒ (s, i - t) } mkString " ") + s" $timerService")
        assert((r map { _._1 }) == Vector("A", "B", "C"))
        assert(r(0)._2 >= t && r(0)._2 <= t + 200.ms)
        assert(r(1)._2 >= t && r(1)._2 <= t + 400.ms)
        assert(r(2)._2 >= t && r(2)._2 <= t + 600.ms)
        assert(timerService.overview == TimerServiceOverview(count = 0, completeCount = nr * 3, wakeCount = nr * 2))
      }
    }
    timerService.close()
    assert(timerService.isEmpty)
  }

  "cancel" in {
    autoClosing(TimerService()) { timerService ⇒
      for (nr ← 1 to 2) {
        val results = new ConcurrentLinkedQueue[(String, Instant)]()
        val t = now
        timerService.delayed(200.ms) onComplete { _ ⇒ results.add("200" → now) }
        val cancelledTimer = timerService.at(t + 400.ms, "test") onElapsed { results.add("400" → now) }
        assert(!cancelledTimer.isCanceled)
        timerService.cancel(cancelledTimer) shouldBe true
        timerService.cancel(cancelledTimer) shouldBe false
        assert(cancelledTimer.isCanceled)
        sleep(700.ms)
        withClue(s"Run $nr: ") {
          val r = results.toVector
          assert((r map { _._1 }) == Vector("200"))
          assert(r(0)._2 >= t + 200.ms && r(0)._2 <= t + 300.ms)
        }
        timerService.cancel(cancelledTimer) shouldBe false // Cancel is idempotent
      }
    }
  }

  "onElapsed" in {
    autoClosing(TimerService()) { timerService ⇒
      val timer = timerService.delay(0.s, "test")
      val myPromise = Promise[Int]()
      val future: Future[Unit] = timer onElapsed { myPromise success 777 }
      assert(future eq timer)
      myPromise.future await 1.s shouldEqual 777
      assert(timer.isCompleted)
      assert(timer.value == Some(Timer.ElapsedFailure))
      assert(myPromise.isCompleted)
      assert(myPromise.future.value == Some(Success(777)))
    }
  }

  "Timer is a Future, ElapsedException" in {
    autoClosing(TimerService()) { timerService ⇒
      val timer = timerService.delay(0.s, "test")
      val recovered = timer recover {
        case _: Timer.ElapsedException ⇒ 777
      }
      recovered await 1.s shouldEqual 777
      assert(timer.value == Some(Timer.ElapsedFailure))
    }
  }

  "completeWith" in {
    autoClosing(TimerService()) { timerService ⇒
      val a = timerService.addTimer(new Timer(now + 100.ms, "test", completeWith = () ⇒ Success(777)))
      sleep(10.ms)
      assert(!a.isCompleted)
      a await 1.s shouldEqual 777
    }
  }

  "fullfilWith and own promise" in {
    autoClosing(TimerService()) { timerService ⇒
      val promise = Promise[Int]()
      val a = timerService.addTimer(new Timer(now + 100.ms, "test", () ⇒ Success(777), promise))
      sleep(10.ms)
      assert(!promise.isCompleted && !a.isCompleted)
      a await 1.s shouldEqual 777
      assert(promise.isCompleted && a.isCompleted)
      assert(promise.future.value == Some(Success(777)))
    }
  }

  "roundUp" in {
    val t = Instant.parse("2000-01-01T00:00:00Z")
    assert(TimerService.roundUp(t + 1.ms, t.toEpochMilli) == t + 10.ms) //1.ms)
    assert(TimerService.roundUp(t + 99.ms, t.toEpochMilli) == t + 100.ms) //99.ms)
    assert(TimerService.roundUp(t + 123.ms, t.toEpochMilli) == t + 130.ms)
    assert(TimerService.roundUp(t + 423.ms, t.toEpochMilli) == t + 430.ms)
    assert(TimerService.roundUp(t + 923.ms, t.toEpochMilli) == t + 950.ms)
    assert(TimerService.roundUp(t + 1234.ms, t.toEpochMilli) == t + 1300.ms)
    assert(TimerService.roundUp(t + 9234.ms, t.toEpochMilli) == t + 9500.ms)
    assert(TimerService.roundUp(t + 12345.ms, t.toEpochMilli) == t + 13000.ms)
  }

  "Massive parallel parallel timer enqueuing" in {
    autoClosing(TimerService()) { timerService ⇒
      timerService.delayed(20.ms) await 1.s  // Start clock thread
      for (_ ← 1 to 10) {
        val counter = new AtomicInteger
        val n = 5000 * sys.runtime.availableProcessors
        val delays = for (_ ← 1 to n) yield Random.nextInt(40).ms
        val stopwatch = new Stopwatch
        for (delay ← delays) Future { timerService.delay(delay, "test") onElapsed { counter.incrementAndGet() }}
        val ok = waitForCondition(2.s, 10.ms) { counter.get == n }
        info("Parallel: " + stopwatch.itemsPerSecondString(n, "Timer"))
        if (!ok) logger.error(s"$counter/$n $timerService")
        assert(counter.get == n)
      }
    }
  }

  "Massive wake clock-thread (JS-1567)" in {
    val n = 1000
    val overview = testSerialTimers(delay = 1.ms, n, "1ms", test = true)
    overview should have ('count(0), 'completeCount(n))
    assert(overview.wakeCount >= n / 4 && overview.wakeCount <= n, "wakeCount")  // Should be nearly n on a fast machine
  }

  "Performance of serial 0ms timer, not short-cut for this test" in {
    testSerialTimers(delay = 0.ms, 10000, "Normal", test = true)
  }

  "Performance of serial 0ms timer" in {
    testSerialTimers(delay = 0.ms, 1000000, "Short-cut")
  }

  private def testSerialTimers(delay: Duration, n: Int, name: String, test: Boolean = false): TimerServiceOverview =
    autoClosing(TimerService(test = test)) { timerService ⇒
      val stopwatch = new Stopwatch
      @volatile var last = now
      val counter = new AtomicInteger(-1)
      val finished = Promise[Unit]()
      def addNextTimer(): Unit = {
        last = now
        if (counter.incrementAndGet() < n) timerService.delay(delay, "test") onElapsed addNextTimer
        else finished.success(())
      }
      addNextTimer()
      Future {
        blocking { waitForCondition(60.s, 1.s) { now > last + 1.s } }
        finished.tryFailure(new RuntimeException(s"STOPPED AFTER $counter"))
      }
      Await.result(finished.future, 60.seconds)
      info(s"$name: " + stopwatch.itemsPerSecondString(n, "Timer"))
      assert(counter.get == n)
      timerService.overview
    }

  "Future.timeoutAfter" in {
    autoClosing(TimerService()) { implicit timerService ⇒
      def newFuture(a: Duration, timeout: Duration) = Future { sleep(a); "OK" } timeoutAfter (timeout, "test")
      newFuture(100.ms, 200.ms) await 1.s shouldEqual "OK"
      intercept[Timer.ElapsedException] {
        newFuture(200.ms, 100.ms) await 1.s
      }

      def newRecoveredFuture(a: Duration, timeout: Duration) = newFuture(a, timeout) recover { case _: Timer.ElapsedException ⇒ "TIMEOUT" }
      Await.ready(newRecoveredFuture(2.ms, 2.ms), 500.millis) // Warm-up
      newRecoveredFuture(100.ms, 200.ms) await 1.s shouldEqual "OK"
      newRecoveredFuture(200.ms, 100.ms) await 1.s shouldEqual "TIMEOUT"
    }
  }

  "TimerService.delayedFuture" in {
    autoClosing(TimerService()) { implicit timerService ⇒
      var executed = false
      val t = now
      val future = timerService.delayedFuture(200.ms) {
        executed = true
        "OK"
      }
      sleep(50.ms)
      assert(!executed)
      future await 1.s shouldEqual "OK"
      assert(t + 100.ms <= now)
    }
  }

  "Future.thenDelay" in {
    autoClosing(TimerService()) { implicit timerService ⇒
      def newFuture(delay: Duration) = Future { "OK" } thenDelay delay
      val t = now
      newFuture(200.ms) await 1.s shouldEqual "OK"
      assert(t + 100.ms <= now)
    }
  }

  "Object.wait" in {
    def meterTime(body: ⇒ Unit) = {
      val t = nanoTime
      body
      Duration.ofNanos(nanoTime - t)
    }
    def test(millis: Iterable[Int]) = Future {
      val lock = new Object
      lock.synchronized {
        for (i ← millis) {
          val t = meterTime { lock.wait(i) }
          val diff = t - Duration.ofMillis(i)
          if (diff <= -1.ms ) logger.warn(s"${i}ms ${diff.pretty}")
          if (diff >= 1.ms) logger.info(s"${i}ms ${diff.pretty}")
        }
      }
    }
    test(List(100, 100, 100))
    val futures = for (range ← List(List(100), List(50, 40, 20, 10), List(50, 40, 20, 3, 2, 1))) yield test(range)
    Await.result(Future.sequence(futures), 600.seconds)
  }

  "ArrayBlockingQueue.offer" in {
    val q = new ArrayBlockingQueue[Boolean](1)
    Stopwatch.measureTime(1000000, "offer") {
      q.offer(true)
      q.take()
    }
    val future = Future { blocking { while(q.take()) {} }}
    Stopwatch.measureTime(10000, "put") {
      q.put(true)
    }
    q.put(false)
    future await 1.s
  }
}

private object TimerServiceTest {
  private val logger = Logger(getClass)
}
