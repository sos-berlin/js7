package com.sos.jobscheduler.common.akkautils

import akka.actor.{Actor, ActorSystem, DeadLetter, DeadLetterSuppression, Props, UnhandledMessage}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.common.akkautils.DeadLetterActor._
import com.sos.jobscheduler.common.scalautil.Logger
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private class DeadLetterActor(output: (=> String) => Unit) extends Actor {
  def receive = {
    case DeadLetter(_: DeadLetterSuppression, _, _) =>
    case UnhandledMessage(_: DeadLetterSuppression, _, _) =>

    case o: DeadLetter =>
      callOutput(s"DeadLetter from ${o.sender} to ${o.recipient}: ${o.message.getClass.scalaName} ${o.message}")

    case o: UnhandledMessage =>
      callOutput(s"UnhandledMessage from ${o.sender} to ${o.recipient}: ${o.message.getClass.scalaName} ${o.message}")
  }

  private def callOutput(string: => String) =
    try output(string)
    catch {
      case NonFatal(t) => logger.warn(t.toString)
      case t: OutOfMemoryError => logger.error(t.toString, t)
    }
}

object DeadLetterActor {
  private val logger = Logger(getClass)

  def subscribe(actorSystem: ActorSystem, output: (=> String) => Unit = logDeadLetter): Unit = {
    val actor = actorSystem.actorOf(props(output), "DeadLetter")
    actorSystem.eventStream.subscribe(actor, classOf[DeadLetter])
    actorSystem.eventStream.subscribe(actor, classOf[UnhandledMessage])
  }

  private def logDeadLetter(message: => String): Unit = logger.warn(message)

  private def props(output: (=> String) => Unit) = Props { new DeadLetterActor(output) }
}
