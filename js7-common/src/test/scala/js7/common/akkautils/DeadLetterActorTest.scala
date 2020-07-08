package js7.common.akkautils

import akka.actor.{Actor, DeadLetterSuppression, Props}
import js7.base.time.ScalaTime._
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.akkautils.DeadLetterActorTest._
import js7.common.configutils.Configs._
import js7.common.time.WaitForCondition.waitForCondition
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class DeadLetterActorTest extends AnyFreeSpec {

  "DeadLetterActor.subscribe" in {
    val actorSystem = newActorSystem(classOf[DeadLetterActorTest].getSimpleName, config"akka.log-dead-letters = 0")
    val buffer = mutable.Buffer[String]()
    DeadLetterActor.subscribe(actorSystem, o => buffer += o)
    val actorRef = actorSystem.actorOf(Props[TestActor]())
    actorRef ! "stop"
    actorRef ! new SuppressedMessage
    actorRef ! new TestMessage
    waitForCondition(10.s, 10.ms) { buffer.size == 1 }
    Akkas.terminateAndWait(actorSystem, 99.s)
    assert(buffer.size == 1)
    assert(buffer.head startsWith "DeadLetter ")
    assert(buffer.head contains classOf[TestMessage].getName)
  }
}

object DeadLetterActorTest {
  private class TestMessage
  private class SuppressedMessage extends DeadLetterSuppression

  private class TestActor extends Actor {
    def receive = {
      case "stop" => context.stop(self)
    }
  }
}
