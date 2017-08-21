package com.sos.jobscheduler.common.akkautils

import akka.actor.SupervisorStrategy.Decider
import akka.actor.{Actor, ActorRef, ActorRefFactory, Props, Terminated}
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Try}

/**
  * Catches the exception of a failing top-level actor.
  * The child should not publish its parent (this actor).
  *
  * @tparam A Result type of the actor, when not failing.
  * @author Joacim Zschimmer
  */
final class CatchingActor[A](
  terminated: Promise[A],
  props: Props,
  decider: Decider = CatchingSupervisorStrategy.defaultDecider,
  onStopped: ActorRef ⇒ Try[A],
  loggingEnabled: Boolean = false)
extends Actor {

  import context.{actorOf, parent, stop, watch}

  override val supervisorStrategy = CatchingSupervisorStrategy[A](terminated, loggingEnabled = loggingEnabled, decider = decider)
  private val child = watch(actorOf(props, "catching"))

  def receive = {
    case Terminated(`child`) ⇒
      if (!terminated.isCompleted) {
        terminated.tryComplete(onStopped(child))
      }
      stop(self)

    case msg if sender() == child ⇒
      parent ! msg

    case msg if sender() == parent ⇒
      child ! msg

    case msg ⇒
      // Any other sender is presumed to address the child, not the parent.
      child.!(msg)(sender())
  }
}

object CatchingActor {
  private def defaultOnStopped(child: ActorRef) = Failure(new StoppedException(child))

  def actorOf[A](props: Promise[A] ⇒ Props, name: String = "", decider: Decider = CatchingSupervisorStrategy.defaultDecider, onStopped: ActorRef ⇒ Try[A] = defaultOnStopped _, loggingEnabled: Boolean = false)
    (implicit actorRefFactory: ActorRefFactory)
  : (ActorRef, Future[A]) = {
    val terminated = Promise[A]()
    val catchingProps = Props { new CatchingActor(terminated, props(terminated), decider, onStopped, loggingEnabled = loggingEnabled) }
    val a = if (name.nonEmpty) actorRefFactory.actorOf(catchingProps, name) else actorRefFactory.actorOf(catchingProps)
    (a, terminated.future)
  }

  final class StoppedException(actorRef: ActorRef)
  extends RuntimeException(s"Actor '${actorRef.path}' stopped unexpectedly")
}
