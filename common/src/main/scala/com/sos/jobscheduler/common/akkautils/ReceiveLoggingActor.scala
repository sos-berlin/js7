package com.sos.jobscheduler.common.akkautils

import com.sos.jobscheduler.common.akkautils.Akkas._
import com.sos.jobscheduler.common.akkautils.ReceiveLoggingActor._
import com.sos.jobscheduler.common.scalautil.Logger

/**
  * @author Joacim Zschimmer
  */
trait ReceiveLoggingActor extends SimpleStateActor
{
  protected val isReceiveLoggingEnabled = context.system.settings.config.getBoolean("jobscheduler.akka.actor-logging")

  protected[ReceiveLoggingActor] def isLoggingEnabled = isReceiveLoggingEnabled && logger.underlying.isDebugEnabled(Logger.Actor)

  override def postStop() = {
    if (isLoggingEnabled) {
      logger.debug(Logger.Actor, s"${context.self.path.pretty} stopped")
    }
    super.postStop()
  }

  abstract override protected def become(state: String)(recv: Receive): Unit =
    if (isLoggingEnabled) {
      logger.debug(Logger.Actor, s"${context.self.path.pretty} becomes $state")
      super.become(state)(debugReceive(recv))
    } else
      super.become(state)(recv)

  private def debugReceive(recv: Receive): Receive = {
    new Receive {
      def isDefinedAt(msg: Any) = recv isDefinedAt msg

      def apply(msg: Any) = {
        logger.debug(Logger.Actor, s"${context.self.path.pretty} receives '$msg' from ${sender().path.pretty}")
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
        logger.debug(Logger.Actor, s"${context.self.path.pretty} stash")
      }
      super.stash()
    }

    override def unstashAll() = {
      if (isLoggingEnabled) {
        logger.debug(Logger.Actor, s"${context.self.path.pretty} unstashAll")
      }
      super.unstashAll()
    }
  }
}
