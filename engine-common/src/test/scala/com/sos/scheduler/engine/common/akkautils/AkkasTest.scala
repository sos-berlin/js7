package com.sos.scheduler.engine.common.akkautils

import akka.actor.ActorSystem
import akka.util.Timeout
import com.sos.scheduler.engine.common.akkautils.Akkas._
import com.typesafe.config.ConfigFactory
import java.util.concurrent.TimeUnit
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class AkkasTest extends FreeSpec {

  "MaximumTimeout" in {
    val millis = Int.MaxValue * 10L - 2000
    assert(millis / 1000 / 3600 / 24 / 30 == 8)  // Months
    assert(MaximumTimeout == Timeout.apply(millis, TimeUnit.MILLISECONDS))
  }

  "maximumTimeout" in {
    val millis = Int.MaxValue * 10L - 2000
    assert(millis / 1000 / 3600 / 24 / 30 == 8)  // Months
    assert(maximumTimeout(ActorSystem().settings) == Timeout.apply(millis, TimeUnit.MILLISECONDS))
  }

  "maximumTimeout with tick-duration = 1s" in {
    val millis = Int.MaxValue * 1000L - 2000
    assert(millis / 1000 / 3600 / 24 / 365 == 68)  // Years
    val config = ConfigFactory.parseString("akka.scheduler.tick-duration = 1s")
    assert(maximumTimeout(ActorSystem("TEST", config).settings) == Timeout.apply(millis, TimeUnit.MILLISECONDS))
  }

  "DummyCancellable" in {
    val c = new DummyCancellable
    assert(!c.isCancelled)
    assert(c.cancel())
    assert(c.isCancelled)
    assert(!c.cancel())
    assert(c.isCancelled)
  }
}
