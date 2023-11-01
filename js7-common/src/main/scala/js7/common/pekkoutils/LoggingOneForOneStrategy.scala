package js7.common.pekkoutils

import java.lang.reflect.InvocationTargetException
import js7.base.log.LogLevel.syntax.*
import js7.base.log.LogLevel.{Debug, Error, Warn}
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkoutils.LoggingOneForOneStrategy.*
import js7.common.pekkoutils.Pekkos.*
import org.apache.pekko.actor.SupervisorStrategy.{Decider, Directive, Escalate, Restart, Resume, Stop, defaultDecider}
import org.apache.pekko.actor.{ActorContext, ActorInitializationException, ActorRef, OneForOneStrategy, SupervisorStrategy}

/**
  * @author Joacim Zschimmer
  */
class LoggingOneForOneStrategy(loggingEnabled: Boolean = true)(decider: Decider)
extends OneForOneStrategy(maxNrOfRetries = 0, loggingEnabled = loggingEnabled)(decider) {

  /**
    * Like the original method, but using Throwable's toString instead of getMEssage, and using ScalaLogger.
    */
  override def logFailure(context: ActorContext, child: ActorRef, throwable: Throwable, decision: Directive): Unit = {
    def logMessage = throwable match {
      case e: ActorInitializationException if e.getCause ne null => e.getCause match {
        case ex: InvocationTargetException if ex.getCause ne null => ex.getCause.toStringWithCauses
        case ex => ex.toStringWithCauses
      }
      case e => e.toStringWithCauses
    }
    val logLevel =
      if (!loggingEnabled)
        Debug
      else
        decision match {
          case Resume   => Warn
          case Escalate => Debug
          case Restart | Stop => Error
          case _ => Error
        }
    logger.log(logLevel, s"$decision ${child.path.pretty}: $logMessage", throwable)
  }
}

object LoggingOneForOneStrategy {
  private val logger = Logger(getClass)

  final val defaultStrategy: SupervisorStrategy = {
    new LoggingOneForOneStrategy()(defaultDecider)
  }
}
