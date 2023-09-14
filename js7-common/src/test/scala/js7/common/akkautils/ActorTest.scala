package js7.common.akkautils

import akka.actor.{Actor, Props}
import js7.base.configutils.Configs.*
import js7.base.log.Logger
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.common.akkautils.ActorTest.*
import monix.execution.atomic.{AtomicBoolean, AtomicInt}
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.Promise
import scala.util.control.NoStackTrace

final class ActorTest extends OurTestSuite with BeforeAndAfterAll with ProvideActorSystem
{
  protected def config = config"akka.actor.guardian-supervisor-strategy = akka.actor.StoppingSupervisorStrategy"

  override def afterAll() = {
    Akkas.terminateAndWait(actorSystem, 99.s)
    super.afterAll()
  }

  "A crashing actor does not automatically restart" in {
    val startCounter = AtomicInt(0)
    val restarted = AtomicBoolean(false)
    val stopped = Promise[Unit]()

    val actor = actorSystem.actorOf(Props {
      new Actor {
        override def preRestart(t: Throwable, message: Option[Any]) = {
          logger.error("preRestart")
          restarted := true
          super.preRestart(t, message)
        }

        override def postRestart(t: Throwable) = {
          logger.error("postRestart")
          restarted := true
          super.postRestart(t)
        }

        override def preStart() = startCounter.increment()

        override def postStop() = stopped.success(())

        def receive = {
          case "CRASH" => throw new RuntimeException("TEST-CRASH") with NoStackTrace
        }
      }
    })

    actor ! "CRASH"
    stopped.future await 99.s
    assert(!restarted.get() && startCounter.get() == 1)
    sleep(1.s)
    assert(!restarted.get() && startCounter.get() == 1)
  }
}

object ActorTest
{
  private val logger = Logger[this.type]
}
