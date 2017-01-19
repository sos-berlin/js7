package com.sos.scheduler.engine.common.akkautils

import akka.actor.{Actor, ActorSystem, Props}
import com.sos.scheduler.engine.common.akkautils.DeadLetterActorTest._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class DeadLetterActorTest extends FreeSpec {

  "DeadLetterActor.subscribe" in {
    val actorSystem = ActorSystem(classOf[DeadLetterActorTest].getSimpleName)
    val buffer = mutable.Buffer[String]()
    DeadLetterActor.subscribe(actorSystem, o ⇒ buffer += o)
    val actorRef = actorSystem.actorOf(Props[TestActor])
    actorRef ! "stop"
    actorRef ! new TestMessage
    waitForCondition(10.s, 10.ms) { buffer.size == 1 }
    actorSystem.shutdown()
    assert(buffer.size == 1)
    assert(buffer(0) startsWith "DeadLetter " + classOf[TestMessage].getName)
  }
}

object DeadLetterActorTest {
  private class TestMessage

  private class TestActor extends Actor {
    def receive = {
      case "stop" ⇒ context.stop(self)
    }
  }
}
