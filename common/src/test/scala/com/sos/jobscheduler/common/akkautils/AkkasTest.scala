package com.sos.jobscheduler.common.akkautils

import akka.actor.{Actor, ActorPath, ActorSystem, Props}
import akka.util.{ByteString, Timeout}
import com.sos.jobscheduler.common.akkautils.Akkas._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.typesafe.config.ConfigFactory
import java.util.concurrent.TimeUnit
import org.scalatest.FreeSpec
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
final class AkkasTest extends FreeSpec {

  "maximumTimeout" in {
    val millis = Int.MaxValue * 10L - 2000
    assert(millis / 1000 / 3600 / 24 / 30 == 8)  // Months
    val actorSystem = ActorSystem("AkkasTest")
    assert(maximumTimeout(actorSystem.settings) == Timeout.apply(millis, TimeUnit.MILLISECONDS))
    actorSystem.terminate()
  }

  "maximumTimeout with tick-duration = 1s" in {
    val millis = Int.MaxValue * 1000L - 2000
    assert(millis / 1000 / 3600 / 24 / 365 == 68)  // Years
    val config = ConfigFactory.parseString("akka.scheduler.tick-duration = 1s")
    val actorSystem = ActorSystem("AkkasTest", config)
    assert(maximumTimeout(actorSystem.settings) == Timeout.apply(millis, TimeUnit.MILLISECONDS))
    actorSystem.terminate()
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
    assert(byteStringToTruncatedString(byteString).length < 330)
  }

  "encodeAsActorName" in {
    intercept[IllegalArgumentException] { encodeAsActorName("") }
    assert(encodeAsActorName("a") == "a")
    assert(encodeAsActorName("$/$") == "%24%2F$")
    assert(encodeAsActorName("%%@") == "%25%25@")
    assert(decodeActorName("%24%2F$") == "$/$")
    assert(encodeAsActorName("folder/subfolder/jobname") == "folder%2Fsubfolder%2Fjobname")
    assert(encodeAsActorName("a?b=!&c=ö[]{}") == "a%3Fb=!&c=%C3%B6%5B%5D%7B%7D")
    assert(encodeAsActorName("()") == "%28%29")

    check((((32 to 127) ++ (160 to 1000)) map { _.toChar }).toString)
    for (_ ← 1 to 10000) {
      check(Vector.fill(100) { 32 + Random.nextInt(95) } .toString)
    }
    def check(string: String): Unit = {
      val actorName = encodeAsActorName(string)
      assert(ActorPath.isValidPathElement(actorName))
      assert(decodeActorName(actorName) == string)
    }
  }

  "SupervisorStrategy" in {
    val actorSystem = ActorSystem("AkkasTest")
    try {
      actorSystem.actorOf(Props { new Actor {
        def receive = {
          case body: (() ⇒ Unit) ⇒ body()
        }
      }})
    }
    finally actorSystem.terminate() await 99.s
  }
}
