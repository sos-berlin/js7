package com.sos.jobscheduler.common.akkautils

import akka.actor.Actor
import com.sos.jobscheduler.common.akkautils.ActorReceiveLogging._
import com.sos.jobscheduler.common.scalautil.Logger

/**
  * @author Joacim Zschimmer
  */
trait ActorReceiveLogging {
  this: Actor ⇒

  protected val isReceiveLoggingEnabled = context.system.settings.config.getBoolean("jobscheduler.logging.actor")

  protected[ActorReceiveLogging] def isLoggingEnabled = isReceiveLoggingEnabled && logger.underlying.isDebugEnabled(Logger.Actor)

  protected def become(state: String)(recv: Receive): Unit =
    if (isLoggingEnabled) {
      logger.debug(Logger.Actor, s"${context.self.path} becomes $state")
      context.become(debugReceive(recv))
    } else
      context.become(recv)

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

object ActorReceiveLogging {
  private val logger = Logger(getClass)

  trait WithStash extends akka.actor.Stash with ActorReceiveLogging
  {
    override def stash() = {
      if (isLoggingEnabled) {
        logger.debug(Logger.Actor, s"${context.self.path} stash")
      }
      super.stash()
    }
  }
}
