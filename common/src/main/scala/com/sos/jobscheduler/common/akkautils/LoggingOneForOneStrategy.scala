package com.sos.jobscheduler.common.akkautils

import akka.actor.SupervisorStrategy.{Decider, Directive, Escalate, Restart, Resume, Stop, defaultDecider}
import akka.actor.{ActorContext, ActorInitializationException, ActorRef, OneForOneStrategy, SupervisorStrategy}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.akkautils.LoggingOneForOneStrategy._
import com.sos.jobscheduler.common.log.LogLevel._
import com.sos.jobscheduler.common.scalautil.Logger
import java.lang.reflect.InvocationTargetException

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
    val logLevel = decision match {
      case _ if !loggingEnabled => Debug
      case Resume   => Warn
      case Escalate => Debug
      case Restart | Stop => Error
    }
    logger.log(logLevel, s"$decision ${child.path}: $logMessage", throwable)
  }
}

object LoggingOneForOneStrategy {
  private val logger = Logger(getClass)

  final val defaultStrategy: SupervisorStrategy = {
    new LoggingOneForOneStrategy()(defaultDecider)
  }
}
