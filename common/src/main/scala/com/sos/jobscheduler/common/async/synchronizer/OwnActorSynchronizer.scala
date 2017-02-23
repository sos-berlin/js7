package com.sos.scheduler.engine.common.async.synchronizer

import akka.actor.{ActorRefFactory, PoisonPill}
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait OwnActorSynchronizer[A] extends AutoCloseable with ActorSynchronizer[A] {

  protected def actorRefFactory: ActorRefFactory

  protected final val actor = actorRefFactory.actorOf(SynchronizerActor.props)
  @volatile private var terminated = false

  def close() = {
    terminated = true
    actor ! PoisonPill
  }

  override def apply(function: ⇒ A): Future[A] = {
    if (terminated) throw new IllegalStateException("Synchronizer actor has been terminated")  // Do not rely on it. It races with close.
    super.apply(function)
  }
}
