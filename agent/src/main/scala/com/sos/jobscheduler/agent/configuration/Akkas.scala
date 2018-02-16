package com.sos.jobscheduler.agent.configuration

import akka.actor.ActorSystem
import com.google.common.io.Closer
import com.sos.jobscheduler.common.akkautils.DeadLetterActor
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.SideEffect._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.typesafe.config.{Config, ConfigFactory}

/**
 * @author Joacim Zschimmer
 */
object Akkas {
  private val ShutdownDuration = 10.s
  private val logger = Logger(getClass)

  def newActorSystem(name: String, config: Config = ConfigFactory.empty)(implicit closer: Closer): ActorSystem =
    ActorSystem(name, config withFallback AgentConfiguration.DefaultsConfig) sideEffect { o ⇒
      DeadLetterActor.subscribe(o)
      closer.onClose {
        logger.debug("ActorSystem terminate")
        o.terminate() await ShutdownDuration
      }
    }
}
