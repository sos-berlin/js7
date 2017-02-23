package com.sos.scheduler.engine.common.async.synchronizer

import akka.actor.{Actor, Props}
import com.sos.scheduler.engine.common.async.synchronizer.SynchronizerActor._
import scala.concurrent.Promise
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
private[synchronizer] final class SynchronizerActor extends Actor {
  def receive = {
    case Execute(function, promise) ⇒ promise tryComplete Try { function() }
  }
}

private[synchronizer] object SynchronizerActor {
  private[synchronizer] final case class Execute[A](function: () ⇒ A, promise: Promise[A])

  private[synchronizer] def props = Props { new SynchronizerActor }
}
