package com.sos.scheduler.engine.common.akkautils

import akka.actor.ActorSystem
import akka.util.{ByteString, Timeout}
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
    val actorSystem = ActorSystem("AkkasTest")
    assert(maximumTimeout(actorSystem.settings) == Timeout.apply(millis, TimeUnit.MILLISECONDS))
    actorSystem.shutdown()
  }

  "maximumTimeout with tick-duration = 1s" in {
    val millis = Int.MaxValue * 1000L - 2000
    assert(millis / 1000 / 3600 / 24 / 365 == 68)  // Years
    val config = ConfigFactory.parseString("akka.scheduler.tick-duration = 1s")
    val actorSystem = ActorSystem("AkkasTest", config)
    assert(maximumTimeout(actorSystem.settings) == Timeout.apply(millis, TimeUnit.MILLISECONDS))
    actorSystem.shutdown()
  }

  "DummyCancellable" in {
    val c = new DummyCancellable
    assert(!c.isCancelled)
    assert(c.cancel())
    assert(c.isCancelled)
    assert(!c.cancel())
    assert(c.isCancelled)
  }

  "byteStringToTruncatedString" in {
    val byteString = ByteString(0, 1, 30, 31)
    assert(byteStringToTruncatedString(byteString) == "4 bytes 00 01 1e 1f")
  }

  "Big byteStringToTruncatedString" in {
    val byteString = ByteString.fromInts(0 until 1000: _*)
    val string = byteStringToTruncatedString(byteString)
    assert(string startsWith "1000 bytes 00 01 02 03 04 ")
    assert(string endsWith " ...")
    assert(byteStringToTruncatedString(byteString).size < 330)
  }

  "encodeAsActorName" in {
    intercept[IllegalArgumentException] { encodeAsActorName("") }
    assert(encodeAsActorName("a") == "a")
    assert(encodeAsActorName("$/$") == "%24%2F$")
    assert(decodeActorName("%24%2F$") == "$/$")
    assert(encodeAsActorName("folder/subfolder/jobname") == "folder%2Fsubfolder%2Fjobname")
    assert(encodeAsActorName("a?b=!&c=ö") == "a%3Fb=!&c=%C3%B6")
  }
}
