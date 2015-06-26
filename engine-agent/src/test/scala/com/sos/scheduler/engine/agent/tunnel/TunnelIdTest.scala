package com.sos.scheduler.engine.agent.tunnel

import com.sos.scheduler.engine.agent.tunnel.TunnelIdTest._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.Stopwatch.measureTime
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TunnelIdTest extends FreeSpec {

  "newPassword" in {
    logger.debug(TunnelId.newPassword().toString)
    val result = measureTime(1000, "newPassword") { TunnelId.newPassword() }
    assert(result.singleDuration < 5.ms)  // Most time, it is 0.1ms
  }

  "Password.toString does not show password" in {
    assert(TunnelId.Password("secret").toString == "Password(...)")
  }
}

private object TunnelIdTest {
  private val logger = Logger(getClass)
}
