package js7.common.pekkoutils

import js7.base.configutils.Configs.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.common.pekkoutils.DeadLetterActorTest.*
import js7.common.pekkoutils.Pekkos.newActorSystem
import org.apache.pekko.actor.{Actor, DeadLetterSuppression, Props}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class DeadLetterActorTest extends OurTestSuite:

  "DeadLetterActor.subscribe" in:
    val actorSystem = newActorSystem(classOf[DeadLetterActorTest].getSimpleName, config"pekko.log-dead-letters = 0")
    val buffer = mutable.Buffer[String]()
    DeadLetterActor.subscribe(actorSystem, (logLevel, msg) => buffer += msg())
    val actorRef = actorSystem.actorOf(Props[TestActor]())
    actorRef ! "stop"
    actorRef ! new SuppressedMessage
    actorRef ! new TestMessage
    waitForCondition(10.s, 10.ms) { buffer.size == 1 }
    Pekkos.terminateAndWait(actorSystem, 99.s)
    assert(buffer.size == 1)
    assert(buffer.head startsWith "DeadLetter ")
    assert(buffer.head contains classOf[TestMessage].getName)

object DeadLetterActorTest:
  private class TestMessage
  private class SuppressedMessage extends DeadLetterSuppression

  private class TestActor extends Actor:
    def receive =
      case "stop" => context.stop(self)
