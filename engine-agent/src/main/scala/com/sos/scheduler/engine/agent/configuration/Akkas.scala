package com.sos.scheduler.engine.agent.configuration

import akka.actor.ActorSystem
import com.google.common.io.Closer
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.SideEffect._
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
object Akkas {
  private val ConfigurationResourcePath = "com/sos/scheduler/engine/agent/configuration/akka.conf"
  private val ShutdownDuration = 5.seconds

  def newActorSystem(name: String)(implicit closer: Closer): ActorSystem =
    ActorSystem(name, ConfigFactory.load(ConfigurationResourcePath)) sideEffect { o ⇒
      closer.onClose {
        o.shutdown()
        o.awaitTermination(ShutdownDuration)
      }
    }
}
