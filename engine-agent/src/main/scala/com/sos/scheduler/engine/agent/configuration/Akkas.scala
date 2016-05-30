package com.sos.scheduler.engine.agent.configuration

import akka.actor.ActorSystem
import com.google.common.io.Closer
import com.sos.scheduler.engine.common.ClassLoaders._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.SideEffect._
import com.sos.scheduler.engine.common.utils.JavaResource
import com.typesafe.config.{Config, ConfigFactory}
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
object Akkas {
  private val ConfigurationResource = JavaResource("com/sos/scheduler/engine/agent/configuration/akka.conf")
  private val ShutdownDuration = 5.seconds

  def newActorSystem(name: String, config: Config = ConfigFactory.empty)(implicit closer: Closer): ActorSystem =
    ActorSystem(name, config withFallback ConfigFactory.load(currentClassLoader, ConfigurationResource.path)) sideEffect { o ⇒
      closer.onClose {
        o.shutdown()
        o.awaitTermination(ShutdownDuration)
      }
    }
}
