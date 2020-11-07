package js7.common.akkautils

import akka.actor.{Actor, ActorSystem, DeadLetter, DeadLetterSuppression, Props, UnhandledMessage}
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkautils.Akkas._
import js7.common.akkautils.DeadLetterActor._
import js7.common.log.LogLevel
import js7.common.log.LogLevel.syntax._
import js7.common.log.LogLevel.{Debug, Warn}
import js7.common.scalautil.Logger
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private class DeadLetterActor(output: (LogLevel, () => String) => Unit) extends Actor
{
  def receive = {
    case DeadLetter(_: DeadLetterSuppression, _, _) =>
    case UnhandledMessage(_: DeadLetterSuppression, _, _) =>

    case o: DeadLetter =>
      callOutput(messageToLogLevel(o.message),
        s"DeadLetter from ${o.sender.path.pretty} to ${o.recipient.path.pretty}: ${o.message.getClass.scalaName} ${o.message}")

    case o: UnhandledMessage =>
      callOutput(messageToLogLevel(o.message),
        s"UnhandledMessage from ${o.sender.path.pretty} to ${o.recipient.path.pretty}: ${o.message.getClass.scalaName} ${o.message}")
  }

  private def callOutput(logLevel: LogLevel, string: => String) =
    try output(logLevel, () => string)
    catch {
      case NonFatal(t) => logger.warn(t.toString)
      case t: OutOfMemoryError => logger.error(t.toString, t)
    }
}

object DeadLetterActor
{
  private val logger = Logger(getClass)

  def subscribe(actorSystem: ActorSystem, output: (LogLevel, () => String) => Unit = logDeadLetter): Unit = {
    val actor = actorSystem.actorOf(props(output), "DeadLetter")
    actorSystem.eventStream.subscribe(actor, classOf[DeadLetter])
    actorSystem.eventStream.subscribe(actor, classOf[UnhandledMessage])
  }

  private def props(output: (LogLevel, () => String) => Unit) =
    Props { new DeadLetterActor(output) }

  private def logDeadLetter(logLevel: LogLevel, message: () => String): Unit = logger.log(logLevel, message())

  private def messageToLogLevel(message: Any): LogLevel =
    message match {
      case _: akka.io.Tcp.Write => Debug
      case _ => Warn
    }
}
