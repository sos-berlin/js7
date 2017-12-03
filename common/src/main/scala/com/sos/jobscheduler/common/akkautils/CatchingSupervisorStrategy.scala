package com.sos.jobscheduler.common.akkautils

import akka.actor.SupervisorStrategy.{Decider, Stop}
import akka.actor.{ActorContext, ActorRef, ChildRestartStats, SupervisorStrategy}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.akkautils.CatchingSupervisorStrategy._
import com.sos.jobscheduler.common.scalautil.Logger
import scala.concurrent.Promise

/**
  * @author Joacim Zschimmer
  */
trait CatchingSupervisorStrategy[A] extends SupervisorStrategy {

  protected def promise: Promise[A]

  abstract override def processFailure(context: ActorContext, restart: Boolean, child: ActorRef, throwable: Throwable,
    stats: ChildRestartStats, children: Iterable[ChildRestartStats]): Unit
  = {
    if (!restart) { // That means SupervisorStrategy.Stop
      if (!promise.tryFailure(new ActorCrashedException(s"Actor '${child.path}' crashed: ${throwable.toStringWithCauses}", throwable))) {
        logger.warn(s"promise.tryFailure failed: $throwable", throwable)
      }
    }
    super.processFailure(context, restart, child, throwable, stats, children)
  }
}

object CatchingSupervisorStrategy {
  private val logger = Logger(getClass)
  val StoppingDecider: Decider = {
    case _ ⇒ Stop
  }
  def defaultDecider = StoppingDecider

  def apply[A](promise: Promise[A], loggingEnabled: Boolean = true, decider: Decider = defaultDecider) = {
    val p = promise
    new LoggingOneForOneStrategy(loggingEnabled = loggingEnabled)(decider)
    with CatchingSupervisorStrategy[A] {
      protected final val promise = p
    }
  }

  final class ActorCrashedException(message: String, cause: Throwable)
  extends RuntimeException(message, cause)
}
