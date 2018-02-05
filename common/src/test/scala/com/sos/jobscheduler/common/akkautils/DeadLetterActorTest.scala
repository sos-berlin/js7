package com.sos.jobscheduler.common.akkautils

import akka.actor.{Actor, ActorSystem, DeadLetterSuppression, Props}
import com.sos.jobscheduler.common.akkautils.DeadLetterActorTest._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class DeadLetterActorTest extends FreeSpec {

  "DeadLetterActor.subscribe" in {
    val actorSystem = ActorSystem(classOf[DeadLetterActorTest].getSimpleName)
    val buffer = mutable.Buffer[String]()
    DeadLetterActor.subscribe(actorSystem, o ⇒ buffer += o)
    val actorRef = actorSystem.actorOf(Props[TestActor])
    actorRef ! "stop"
    actorRef ! new SuppressedMessage
    actorRef ! new TestMessage
    waitForCondition(10.s, 10.ms) { buffer.size == 1 }
    actorSystem.terminate()
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
      case "stop" ⇒ context.stop(self)
    }
  }
}
