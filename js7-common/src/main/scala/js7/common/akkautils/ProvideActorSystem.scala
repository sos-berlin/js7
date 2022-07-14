package js7.common.akkautils

import com.typesafe.config.Config
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.time.ScalaTime.*
import js7.base.utils.Closer.syntax.*
import js7.base.utils.HasCloser
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.akkautils.ProvideActorSystem.*

/**
  * @author Joacim Zschimmer
  */
trait ProvideActorSystem extends HasCloser
{
  protected def actorSystemName: String = getClass.simpleScalaName
  protected def config: Config

  protected final lazy val actorSystem =
    newActorSystem(actorSystemName, config.withFallback(defaultConfig))
      .withCloser { o =>
        if (!o.whenTerminated.isCompleted) {
          Akkas.terminateAndWait(o, TerminationTimeout)
        }
      }
}

object ProvideActorSystem
{
  private val TerminationTimeout = 60.s

  private val defaultConfig = config"""
    #akka.stdout-loglevel = "ERROR"
    #akka.loggers = ["akka.event.slf4j.Slf4jLogger"]
    #akka.loglevel = DEBUG
    #akka.logging-filter = "akka.event.slf4j.Slf4jLoggingFilter"
    akka.logger-startup-timeout = 30s
    """
}
