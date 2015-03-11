package com.sos.scheduler.engine.agent.configuration

import akka.actor.ActorSystem
import com.google.common.io.Closer
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.SideEffect._
import com.typesafe.config.ConfigFactory

/**
 * @author Joacim Zschimmer
 */
object Akkas {
  private val ConfigurationResourcePath = "com/sos/scheduler/engine/agent/configuration/akka.conf"

  def newActorSystem(name: String)(implicit closer: Closer): ActorSystem =
    ActorSystem(name, ConfigFactory.load(ConfigurationResourcePath)) sideEffect { o ⇒ closer.onClose { o.shutdown() } }
}
