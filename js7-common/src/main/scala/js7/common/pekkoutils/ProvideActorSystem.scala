package js7.common.pekkoutils

import com.typesafe.config.Config
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.time.ScalaTime.*
import js7.base.utils.Closer.syntax.*
import js7.base.utils.HasCloser
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkoutils.Pekkos.newActorSystem
import js7.common.pekkoutils.ProvideActorSystem.*

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
          Pekkos.terminateAndWait(o, TerminationTimeout)
        }
      }
}

object ProvideActorSystem
{
  private val TerminationTimeout = 60.s

  private val defaultConfig = config"""
    #pekko.stdout-loglevel = "ERROR"
    #pekko.loggers = ["org.apache.pekko.event.slf4j.Slf4jLogger"]
    #pekko.loglevel = DEBUG
    #pekko.logging-filter = "org.apache.pekko.event.slf4j.Slf4jLoggingFilter"
    pekko.logger-startup-timeout = 30s
    """
}
