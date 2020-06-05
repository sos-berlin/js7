package js7.common.akkautils

import akka.actor.{Actor, ActorPath, ActorSystem, Props}
import akka.util.{ByteString, Timeout}
import js7.base.time.ScalaTime._
import js7.common.akkautils.Akkas._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import com.typesafe.config.ConfigFactory
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import scala.util.Random
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class AkkasTest extends AnyFreeSpec
{
  "maximumTimeout" in {
    val millis = Int.MaxValue * 10L - 2000
    assert(millis / 1000 / 3600 / 24 / 30 == 8)  // Months
    val actorSystem = ActorSystem("AkkasTest")
    assert(maximumTimeout(actorSystem.settings) == Timeout.apply(millis, MILLISECONDS))
    actorSystem.terminate()
  }

  "maximumTimeout with tick-duration = 1s" in {
    val millis = Int.MaxValue * 1000L - 2000
    assert(millis / 1000 / 3600 / 24 / 365 == 68)  // Years
    val config = ConfigFactory.parseString("akka.scheduler.tick-duration = 1s")
    val actorSystem = ActorSystem("AkkasTest", config)
    assert(maximumTimeout(actorSystem.settings) == Timeout.apply(millis, MILLISECONDS))
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
    assert(encodeAsActorName("åЮ") == "%C3%A5%D0%AE")
    assert(encodeAsActorName("🔺") == "%F0%9F%94%BA")
    assert(encodeAsActorName("🥕") == "%F0%9F%A5%95")
    assert(decodeActorName("%C3%A5%D0%AE") == "åЮ")
    assert(decodeActorName("%24%2F$") == "$/$")
    assert(encodeAsActorName("folder/subfolder/jobname") == "folder%2Fsubfolder%2Fjobname")
    assert(encodeAsActorName("a?b=!&c=ö[]{}") == "a%3Fb=!&c=%C3%B6%5B%5D%7B%7D")
    assert(encodeAsActorName("()") == "%28%29")

    check((((32 to 127) ++ (160 to 1000)) map { _.toChar }).toString)
    for (_ <- 1 to 10000) {
      check(Vector.fill(100) { 32 + Random.nextInt(95) } .toString)
    }
    def check(string: String): Unit = {
      val actorName = encodeAsActorName(string)
      assert(ActorPath.isValidPathElement(actorName))
      assert(decodeActorName(actorName) == string)
    }
  }

  "AkkaPath pretty" in {
    assert(ActorPath.fromString("akka://ActorSystem/a/b/c").pretty == "akka://ActorSystem/a/b/c")
    assert(ActorPath.fromString("akka://ActorSystem/a/b").pretty == "akka://ActorSystem/a/b")
    assert(ActorPath.fromString("akka://ActorSystem/%24%2F$").pretty == "akka://ActorSystem/$/$")
    assert(ActorPath.fromString("akka://ActorSystem/%25%25@").pretty == "akka://ActorSystem/%%@")
    assert(ActorPath.fromString("akka://ActorSystem/%C3%A5%D0%AE").pretty == "akka://ActorSystem/åЮ")
    assert(ActorPath.fromString("akka://ActorSystem/folder%2Fsubfolder%2Fjobname").pretty == "akka://ActorSystem/folder/subfolder/jobname")
    assert(ActorPath.fromString("akka://ActorSystem/a%3Fb=!&c=%C3%B6%5B%5D%7B%7D").pretty == "akka://ActorSystem/a?b=!&c=ö[]{}")
    assert(ActorPath.fromString("akka://ActorSystem/%28%29").pretty == "akka://ActorSystem/()")
    assert(ActorPath.fromString("akka://ActorSystem/%F0%9F%94%BA/%F0%9F%A5%95").pretty == "akka://ActorSystem/🔺/🥕")
  }

  "SupervisorStrategy" in {
    val actorSystem = ActorSystem("AkkasTest")
    try {
      actorSystem.actorOf(Props { new Actor {
        def receive = {
          case body: Function0[_] => body()
        }
      }})
    }
    finally actorSystem.terminate() await 99.s
  }

  "actorSystemResource" in {
    var _actorSystem: ActorSystem = null
    actorSystemResource("AkkasTest")
      .use(actorSystem => Task {
        _actorSystem = actorSystem
      })
      .await(99.s)
    assert(_actorSystem.whenTerminated.value.get.get.isInstanceOf[akka.actor.Terminated])
  }
}
