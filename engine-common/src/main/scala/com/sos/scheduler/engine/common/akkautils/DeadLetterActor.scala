package com.sos.scheduler.engine.common.akkautils

import akka.actor.{Actor, ActorSystem, DeadLetter, Props}
import com.sos.scheduler.engine.common.akkautils.DeadLetterActor._
import com.sos.scheduler.engine.common.scalautil.Logger

/**
  * @author Joacim Zschimmer
  */
private class DeadLetterActor extends Actor {
  def receive = {
    case o: DeadLetter ⇒ logger.debug(s"DeadLetter ${o.message.getClass.getName} from ${o.sender} to ${o.recipient}")
  }
}

object DeadLetterActor {
  private val logger = Logger(getClass)

  def subscribe(actorSystem: ActorSystem): Unit =
    actorSystem.eventStream.subscribe(actorSystem.actorOf(props, "DeadLetter"), classOf[DeadLetter])

  private def props = Props { new DeadLetterActor }
}
