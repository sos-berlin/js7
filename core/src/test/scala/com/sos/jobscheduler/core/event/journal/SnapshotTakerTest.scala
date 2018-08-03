package com.sos.jobscheduler.core.event.journal

import akka.actor.{Actor, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.common.akkautils.Akkas.newActorSystem
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition
import com.typesafe.config.ConfigFactory
import io.circe.Encoder
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class SnapshotTakerTest extends FreeSpec
{
  private implicit val askTimeout = Timeout(99.seconds)

  "snapshot-log-period" in {
    val actorSystem = newActorSystem(getClass.simpleScalaName)
    try {
      val config = ConfigFactory.parseString("""
          |jobscheduler.journal {
          |  snapshot-log-period = 10.ms
          |  snapshot-log-actor-limit = 1
          |}
        """.stripMargin)
      val actor = actorSystem.actorOf(
        Props {
          new Actor {
            def receive = Actor.emptyBehavior  // Ignore GetSnapshot
          }
        },
        "TEST-ACTOR")
      val snapshotWriter = actorSystem.actorOf(Props { new SnapshotTaker(_ ⇒ {}, Set(actor), null.asInstanceOf[Encoder[Any]], config, Scheduler.global) })
      def getTestLogCount = (snapshotWriter ? "getTestLogCount").mapTo[Int] await 99.s
      WaitForCondition.waitForCondition(10.s, 10.ms)(getTestLogCount >= 2)
      assert(getTestLogCount >= 2)
    }
    finally actorSystem.terminate() await 99.s
  }
}
