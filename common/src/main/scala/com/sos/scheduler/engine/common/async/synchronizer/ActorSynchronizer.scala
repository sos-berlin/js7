package com.sos.scheduler.engine.common.async.synchronizer

import akka.actor.ActorRef
import scala.concurrent.{Future, Promise}

/**
  * Synchronizes calls with an actor.
  *
  * @author Joacim Zschimmer
  */
trait ActorSynchronizer[A] extends ((⇒ A) ⇒ Future[A]) {

  protected def actor: ActorRef

  def apply(function: ⇒ A): Future[A] = {
    val promise = Promise[A]()
    actor ! SynchronizerActor.Execute(() ⇒ function, promise)
    promise.future
  }
}
