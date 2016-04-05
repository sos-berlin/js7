package com.sos.scheduler.engine.common.akkautils

import akka.actor.{Actor, ActorSystem, DeadLetter, Props}
import com.sos.scheduler.engine.common.scalautil.Logger

/**
  * @author Joacim Zschimmer
  */
private class DeadLetterActor(output: (⇒ String) ⇒ Unit) extends Actor {
  def receive = {
    case o: DeadLetter ⇒ output(s"DeadLetter ${o.message.getClass.getName} from ${o.sender} to ${o.recipient}")
  }
}

object DeadLetterActor {
  private val logger = Logger(getClass)

  def subscribe(actorSystem: ActorSystem, output: (⇒ String) ⇒ Unit = logDeadLetter): Unit =
    actorSystem.eventStream.subscribe(actorSystem.actorOf(props(output), "DeadLetter"), classOf[DeadLetter])

  private def logDeadLetter(message: ⇒ String): Unit = logger.debug(message)

  private def props(output: (⇒ String) ⇒ Unit) = Props { new DeadLetterActor(output) }
}
