package com.sos.scheduler.engine.tunnel

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.Stopwatch.measureTime
import com.sos.scheduler.engine.tunnel.TunnelIdTest._
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
    assert(result.singleDuration < 1.ms)
  }

  "Password.toString does not show password" in {
    assert(TunnelId.Password("secret").toString == "Password(...)")
  }
}

private object TunnelIdTest {
  private val logger = Logger(getClass)
}
