package com.sos.jobscheduler.tunnel.server

import akka.agent.Agent
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch

/**
 * @author Joacim Zschimmer
 */
final class AkkaAgentTest extends FreeSpec {

  "speed" in {
    val n = 100000
    for (_ ← 1 to 5) {
      val a = Agent[Int](0)
      val stopwatch = new Stopwatch
      for (i ← 1 to n) a.send { _ + i }
      val result = a.future await 99.s
      info(stopwatch.itemsPerSecondString(n, "sends"))
      assert(result == (1 to n).sum)
    }
  }
}
