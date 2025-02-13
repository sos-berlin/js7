package js7.common.pekkoutils

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.test.{OurTestSuite}
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.common.pekkoutils.Pekkos.*
import org.apache.pekko
import org.apache.pekko.actor.{Actor, ActorPath, ActorSystem, Props}
import org.apache.pekko.util.ByteString
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
final class PekkosTest extends OurTestSuite:

  private given IORuntime = ioRuntime

  "byteStringToTruncatedString" in:
    val byteString = ByteString(0, 1, 30, 31)
    assert(byteStringToTruncatedString(byteString) == "4 bytes 00 01 1e 1f")

  "Big byteStringToTruncatedString" in:
    val byteString = ByteString.fromInts((0 until 1000)*)
    val string = byteStringToTruncatedString(byteString)
    assert(string.startsWith("1000 bytes 00 01 02 03 04 "))
    assert(string.endsWith(" ..."))
    assert(byteStringToTruncatedString(byteString).length < 330)

  "encodeAsActorName" in:
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

    check(((32 to 127) ++ (160 to 1000)).map(_.toChar).toString)
    for _ <- 1 to 10000 do
      check(Vector.fill(100) { 32 + Random.nextInt(95) } .toString)
    def check(string: String): Unit =
      val actorName = encodeAsActorName(string)
      assert(ActorPath.isValidPathElement(actorName))
      assert(decodeActorName(actorName) == string)

  "PekkoPath pretty" in:
    assert(ActorPath.fromString("pekko://ActorSystem/a/b/c").pretty == "pekko://ActorSystem/a/b/c")
    assert(ActorPath.fromString("pekko://ActorSystem/a/b").pretty == "pekko://ActorSystem/a/b")
    assert(ActorPath.fromString("pekko://ActorSystem/%24%2F$").pretty == "pekko://ActorSystem/$/$")
    assert(ActorPath.fromString("pekko://ActorSystem/%25%25@").pretty == "pekko://ActorSystem/%%@")
    assert(ActorPath.fromString("pekko://ActorSystem/%C3%A5%D0%AE").pretty == "pekko://ActorSystem/åЮ")
    assert(ActorPath.fromString("pekko://ActorSystem/folder%2Fsubfolder%2Fjobname").pretty == "pekko://ActorSystem/folder/subfolder/jobname")
    assert(ActorPath.fromString("pekko://ActorSystem/a%3Fb=!&c=%C3%B6%5B%5D%7B%7D").pretty == "pekko://ActorSystem/a?b=!&c=ö[]{}")
    assert(ActorPath.fromString("pekko://ActorSystem/%28%29").pretty == "pekko://ActorSystem/()")
    assert(ActorPath.fromString("pekko://ActorSystem/%F0%9F%94%BA%7C%F0%9F%A5%95").pretty == "pekko://ActorSystem/🔺|🥕")

  "SupervisorStrategy" in:
    val actorSystem = newActorSystem("PekkosTest", executionContext = ioRuntime.compute)
    try
      actorSystem.actorOf(Props { new Actor {
        def receive = {
          case body: Function0[?] => body()
        }
      }})
    finally Pekkos.terminateAndWait(actorSystem, 99.s)

  "actorSystemResource" in:
    var _actorSystem: ActorSystem | Null = null
    actorSystemResource("PekkosTest").use: actorSystem =>
      IO:
        _actorSystem = actorSystem
    .await(99.s)
    assert(_actorSystem.nn.whenTerminated.value.get.get.isInstanceOf[pekko.actor.Terminated])
