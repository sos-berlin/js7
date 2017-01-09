package com.sos.scheduler.engine.common.akkautils

import akka.actor.{Actor, ActorSystem, DeadLetter, Props, UnhandledMessage}
import com.sos.scheduler.engine.common.scalautil.Logger

/**
  * @author Joacim Zschimmer
  */
private class DeadLetterActor(output: (⇒ String) ⇒ Unit) extends Actor {
  def receive = {
    case o: DeadLetter ⇒
      output(s"DeadLetter ${o.message.getClass.getName} from ${o.sender} to ${o.recipient}")

    case o: UnhandledMessage ⇒
      output(s"UnhandledMessage ${o.message.getClass.getName} from ${o.sender} to ${o.recipient}")
  }
}

object DeadLetterActor {
  private val logger = Logger(getClass)

  def subscribe(actorSystem: ActorSystem, output: (⇒ String) ⇒ Unit = logDeadLetter): Unit = {
    val actor = actorSystem.actorOf(props(output), "DeadLetter")
    actorSystem.eventStream.subscribe(actor, classOf[DeadLetter])
    actorSystem.eventStream.subscribe(actor, classOf[UnhandledMessage])
  }

  private def logDeadLetter(message: ⇒ String): Unit = logger.debug(message)

  private def props(output: (⇒ String) ⇒ Unit) = Props { new DeadLetterActor(output) }
}
