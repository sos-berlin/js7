package com.sos.scheduler.engine.common.time

import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition._
import com.sos.scheduler.engine.common.time.WaitForConditionTest._
import java.lang.System.currentTimeMillis
import java.time.Duration
import java.time.Instant.now
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

final class WaitForConditionTest extends FreeSpec {

  "realTimeIterator (time-critical test)" in {
    realTimeIterator(Seq(now().toEpochMilli)) // Aufruf zum Warmwerden. Laden der Klasse kann eine Weile dauern
    meterElapsedTime { realTimeIterator(Seq((now() + 10.s).toEpochMilli)) } should be < 300.ms // Bereitstellung soll nicht warten
    val t0 = now().toEpochMilli
    val (t1, t2, t3) = (t0 + 500, t0 + 1500, t0 + 2000)
    val i = realTimeIterator(Seq(t1, t2, t3))
    meterElapsedTime { i.next() } .toMillis should be (t1 - t0 +- 400)
    meterElapsedTime { i.next() } .toMillis should be (t2 - t1 +- 400)
    meterElapsedTime { i.next() } .toMillis should be (t3 - t2 +- 400)
  }

  "waitForCondition(TimeoutWithSteps) 0 steps (time-critical test)" in {
    val elapsed = meterElapsedTime { waitForCondition(TimeoutWithSteps(2.s, 1.s)) { true } }
    elapsed should be < 500.ms
  }

  "waitForCondition(TimeoutWithSteps) all steps (time-critical test)" in {
    val elapsed = meterElapsedTime { waitForCondition(TimeoutWithSteps(300.ms, 1.ms)) { false } }
    elapsed should (be >= 300.ms and be <= 700.ms)
  }

  "waitForCondition(TimeoutWithSteps) some steps (time-critical test)" in {
    var cnt = 0
    val elapsed = meterElapsedTime { waitForCondition(TimeoutWithSteps(1.s, 10.ms)) { cnt += 1; cnt > 10 } }  // 100ms
    elapsed should (be >= 100.ms and be < 400.ms)
  }
}

private object WaitForConditionTest {
  def meterElapsedTime(f: ⇒ Unit): Duration = {
    val start = currentTimeMillis()
    f
    (currentTimeMillis() - start).ms
  }
}
