package com.sos.jobscheduler.common.akkautils

import akka.actor.{Actor, Props}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.akkautils.ActorTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.typesafe.config.ConfigFactory
import monix.execution.atomic.{AtomicBoolean, AtomicInt}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Promise
import scala.util.control.NoStackTrace

final class ActorTest extends AnyFreeSpec with BeforeAndAfterAll with ProvideActorSystem
{
  protected def config = ConfigFactory.empty

  override def afterAll() = {
    actorSystem.terminate() await 99.s
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

        override def preStart = startCounter.increment()

        override def postStop = stopped.success(())

        def receive = {
          case "CRASH" => throw new RuntimeException("TEST-CRASH") with NoStackTrace
        }
      }
    })

    actor ! "CRASH"
    stopped.future await 99.s
    assert(!restarted.get && startCounter.get == 1)
    sleep(1.s)
    assert(!restarted.get && startCounter.get == 1)
  }
}

object ActorTest
{
  private val logger = Logger(getClass)
}
