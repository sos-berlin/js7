package com.sos.scheduler.engine.common.async

import com.sos.scheduler.engine.common.time.ScalaJoda.{DurationRichInt, RichInstant, sleep}
import org.joda.time.Instant.now
import org.junit.runner.RunWith
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FreeSpec, OneInstancePerTest}

@RunWith(classOf[JUnitRunner])
final class CallRunnerTest extends FreeSpec with OneInstancePerTest {
  private val callDispatcher = new CallRunner(new StandardCallQueue)

  "(Warm up)" in {  // Für die langsamen SOS-Rechner
    callDispatcher.queue add TimedCall(now()) {}
    callDispatcher.executeMatureCalls()
    1 shouldEqual 1
  }

  "CallRunner runs TimedCall at scheduled instants" in {
    val delay = 500.ms
    var a = 0
    callDispatcher.queue { a += 1 }
    callDispatcher.queue { a += 2 }
    callDispatcher.queue add TimedCall(now() + delay) { a += 10 }
    callDispatcher.queue { a += 100 }
    a shouldEqual 0
    callDispatcher.executeMatureCalls()
    a shouldEqual 103
    sleep(delay)
    callDispatcher.executeMatureCalls()
    a shouldEqual 113
  }
}
