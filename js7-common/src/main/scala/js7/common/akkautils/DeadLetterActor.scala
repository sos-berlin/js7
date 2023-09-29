package js7.common.akkautils

import akka.actor.{Actor, ActorSystem, DeadLetter, DeadLetterSuppression, Props, ReceiveTimeout, UnhandledMessage}
import js7.base.log.LogLevel.{Debug, Warn}
import js7.base.log.Logger.syntax.*
import js7.base.log.{LogLevel, Logger}
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.akkautils.Akkas.*
import js7.common.akkautils.DeadLetterActor.*
import scala.math.Ordering.Implicits.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private class DeadLetterActor(output: (LogLevel, () => String) => Unit) extends Actor:
  def receive =
    case DeadLetter(_: DeadLetterSuppression, _, _) =>
    case UnhandledMessage(_: DeadLetterSuppression, _, _) =>

    case o: DeadLetter =>
      callOutput(messageToLogLevel(o.message),
        s"DeadLetter from ${o.sender.path.pretty} to ${o.recipient.path.pretty}: " +
          s"${o.message.getClass.scalaName} ${o.message.toString.truncateWithEllipsis(1000, showLength = true)}")

    case o: UnhandledMessage =>
      callOutput(messageToLogLevel(o.message),
        s"UnhandledMessage from ${o.sender.path.pretty} to ${o.recipient.path.pretty}: " +
          s"${o.message.getClass.scalaName} ${o.message.toString.truncateWithEllipsis(1000, showLength = true)}")

  private def callOutput(logLevel: LogLevel, string: => String) =
    try output(logLevel, () => string)
    catch
      case NonFatal(t) => logger.warn(t.toString)
      case t: OutOfMemoryError => logger.error(t.toString, t)

object DeadLetterActor:
  private val logger = Logger[this.type]

  def subscribe(actorSystem: ActorSystem, output: (LogLevel, () => String) => Unit = logDeadLetter): Unit =
    val actor = actorSystem.actorOf(props(output), "DeadLetter")
    actorSystem.eventStream.subscribe(actor, classOf[DeadLetter])
    actorSystem.eventStream.subscribe(actor, classOf[UnhandledMessage])

  private def props(output: (LogLevel, () => String) => Unit) =
    Props { new DeadLetterActor(output) }

  private def logDeadLetter(logLevel: LogLevel, message: () => String): Unit =
    logger.log(logLevel, ((logLevel < LogLevel.Info) ?? "❓") + message())

  private def messageToLogLevel(message: Any): LogLevel =
    message match
      case _: akka.io.Tcp.Write => Debug
      case ReceiveTimeout => Debug
      case _ => Warn
