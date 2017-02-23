package com.sos.jobscheduler.agent.configuration

import akka.actor.ActorSystem
import com.google.common.io.Closer
import com.sos.jobscheduler.common.akkautils.DeadLetterActor
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.SideEffect._
import com.typesafe.config.{Config, ConfigFactory}
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
object Akkas {
  private val ShutdownDuration = 5.seconds

  def newActorSystem(name: String, config: Config = ConfigFactory.empty)(implicit closer: Closer): ActorSystem =
    ActorSystem(name, config withFallback AgentConfiguration.DefaultsConfig) sideEffect { o ⇒
      DeadLetterActor.subscribe(o)
      closer.onClose {
        o.shutdown()
        o.awaitTermination(ShutdownDuration)
      }
    }
}
