package js7.agent.configuration

import akka.actor.ActorSystem
import js7.base.time.ScalaTime._
import js7.base.utils.Closer
import js7.base.utils.ScalazStyle._
import js7.base.utils.SideEffect._
import js7.common.akkautils.DeadLetterActor
import js7.common.scalautil.Futures.implicits.SuccessFuture
import js7.common.scalautil.Logger
import js7.common.time.JavaTimeConverters._
import com.typesafe.config.{Config, ConfigFactory}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Deadline.now

/**
 * @author Joacim Zschimmer
 */
object Akkas {
  private val logger = Logger(getClass)

  def newAgentActorSystem(name: String, config: Config = ConfigFactory.empty, defaultExecutionContext: ExecutionContext = ExecutionContext.global)
    (implicit closer: Closer)
  : ActorSystem = {
    val myConfig = config withFallback AgentConfiguration.DefaultConfig
    val ec = myConfig.getBoolean("jobscheduler.akka.use-jobscheduler-thread-pool") ? defaultExecutionContext
    ActorSystem(name, config = Some(myConfig), defaultExecutionContext = ec) sideEffect { o =>
      DeadLetterActor.subscribe(o)
      closer.onClose {
        logger.debug(s"ActorSystem('${o.name}') terminate")
        val since = now
        o.terminate() await myConfig.getDuration("jobscheduler.akka.shutdown-timeout").toFiniteDuration
        logger.debug(s"ActorSystem('${o.name}') terminated (${since.elapsed.pretty})")
      }
    }
  }
}
