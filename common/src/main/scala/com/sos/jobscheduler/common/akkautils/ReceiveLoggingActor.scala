package com.sos.jobscheduler.common.akkautils

import com.sos.jobscheduler.common.akkautils.ReceiveLoggingActor._
import com.sos.jobscheduler.common.scalautil.Logger

/**
  * @author Joacim Zschimmer
  */
trait ReceiveLoggingActor extends SimpleStateActor
{
  protected val isReceiveLoggingEnabled = context.system.settings.config.getBoolean("jobscheduler.logging.actor")

  protected[ReceiveLoggingActor] def isLoggingEnabled = isReceiveLoggingEnabled && logger.underlying.isDebugEnabled(Logger.Actor)

  abstract override protected def become(state: String)(recv: Receive): Unit =
    if (isLoggingEnabled) {
      logger.debug(Logger.Actor, s"${context.self.path} becomes $state")
      super.become(state)(debugReceive(recv))
    } else
      super.become(state)(recv)

  private def debugReceive(recv: Receive): Receive = {
    new Receive {
      def isDefinedAt(msg: Any) = recv isDefinedAt msg

      def apply(msg: Any) = {
        logger.debug(Logger.Actor, s"${context.self.path} receives '$msg' from ${sender().path}")
        recv(msg)
      }
    }
  }
}

object ReceiveLoggingActor {
  private val logger = Logger(getClass)

  trait WithStash extends akka.actor.Stash with ReceiveLoggingActor
  {
    override def stash() = {
      if (isLoggingEnabled) {
        logger.debug(Logger.Actor, s"${context.self.path} stash")
      }
      super.stash()
    }
  }
}
