package js7.common.pekkoutils

import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkoutils.CatchingSupervisorStrategy.*
import js7.common.pekkoutils.Pekkos.*
import org.apache.pekko.actor.SupervisorStrategy.{Decider, Stop}
import org.apache.pekko.actor.{ActorContext, ActorRef, ChildRestartStats, SupervisorStrategy}
import scala.concurrent.Promise

/**
  * @author Joacim Zschimmer
  */
trait CatchingSupervisorStrategy[A] extends SupervisorStrategy:

  protected def promise: Promise[A]

  abstract override def processFailure(context: ActorContext, restart: Boolean, child: ActorRef, throwable: Throwable,
    stats: ChildRestartStats, children: Iterable[ChildRestartStats]): Unit
  =
    if !restart then // That means SupervisorStrategy.Stop
      if !promise.tryFailure(new ActorTerminatedException(s"Actor '${child.path.pretty}' terminated due to error: ${throwable.toStringWithCauses}", throwable)) then
        logger.warn(s"promise.tryFailure failed: $throwable", throwable)
    super.processFailure(context, restart, child, throwable, stats, children)


object CatchingSupervisorStrategy:
  private val logger = Logger[this.type]
  val StoppingDecider: Decider =
    case _ => Stop
  def defaultDecider: Decider = StoppingDecider

  def apply[A](
    promise: Promise[A], 
    loggingEnabled: Boolean = true, 
    decider: Decider = defaultDecider)
  : LoggingOneForOneStrategy & CatchingSupervisorStrategy[A] =
    val p = promise
    new LoggingOneForOneStrategy(loggingEnabled = loggingEnabled)(decider)
    with CatchingSupervisorStrategy[A]:
      protected final val promise = p

  final class ActorTerminatedException(message: String, cause: Throwable)
  extends RuntimeException(message, cause)
