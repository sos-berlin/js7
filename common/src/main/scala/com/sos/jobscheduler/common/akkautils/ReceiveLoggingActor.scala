package com.sos.jobscheduler.common.akkautils

import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.common.akkautils.Akkas._
import com.sos.jobscheduler.common.akkautils.ReceiveLoggingActor._
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.log.LogLevel
import com.sos.jobscheduler.common.log.LogLevel._
import com.sos.jobscheduler.common.scalautil.Logger

/**
  * @author Joacim Zschimmer
  */
trait ReceiveLoggingActor extends SimpleStateActor
{
  protected[ReceiveLoggingActor] val receiveLogLevel =
    context.system.settings.config.as[LogLevel]("jobscheduler.akka.actor-message-log-level")

  protected[ReceiveLoggingActor] def isLoggingEnabled =
    logger.underlying.isEnabled(receiveLogLevel, Logger.Actor)

  override def postStop() = {
    logger.log(receiveLogLevel, Logger.Actor, s"${context.self.path.pretty} stopped")
    super.postStop()
  }

  abstract override protected def become(state: String)(recv: Receive): Unit =
    if (isLoggingEnabled) {
      logger.log(receiveLogLevel, Logger.Actor, s"${context.self.path.pretty} becomes $state")
      super.become(state)(debugReceive(recv))
    } else
      super.become(state)(recv)

  private def debugReceive(recv: Receive): Receive = {
    new Receive {
      def isDefinedAt(msg: Any) = recv isDefinedAt msg

      def apply(msg: Any) = {
        logger.log(receiveLogLevel, Logger.Actor,
          s"${context.self.path.pretty} receives '${msg.toString.takeWhile(_ != '\n').truncateWithEllipsis(200)}' from ${sender().path.pretty}")
        recv(msg)
      }

      override def applyOrElse[A1, B1 >: Unit](x: A1, default: A1 => B1): B1 =
        if (isLoggingEnabled) {
          super.applyOrElse(x, default)
        } else
          recv.applyOrElse(x, default)
    }
  }
}

object ReceiveLoggingActor {
  private val logger = Logger(getClass)

  trait WithStash extends akka.actor.Stash with ReceiveLoggingActor
  {
    override def stash() = {
      if (isLoggingEnabled) {
        logger.log(receiveLogLevel, Logger.Actor, s"${context.self.path.pretty} stash")
      }
      super.stash()
    }

    override def unstashAll() = {
      if (isLoggingEnabled) {
        logger.log(receiveLogLevel, Logger.Actor, s"${context.self.path.pretty} unstashAll")
      }
      super.unstashAll()
    }
  }
}
