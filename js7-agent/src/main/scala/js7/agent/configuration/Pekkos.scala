package js7.agent.configuration

import com.typesafe.config.{Config, ConfigFactory}
import js7.base.utils.Closer
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkoutils.DeadLetterActor
import js7.common.pekkoutils.Pekkos.terminateAndWait
import org.apache.pekko.actor.ActorSystem
import scala.concurrent.ExecutionContext
import scala.util.chaining.scalaUtilChainingOps

/**
 * @author Joacim Zschimmer
 */
object Pekkos
{
  def newAgentActorSystem(name: String, config: Config = ConfigFactory.empty, defaultExecutionContext: ExecutionContext = ExecutionContext.global)
    (implicit closer: Closer)
  : ActorSystem = {
    val myConfig = config
      .withFallback(AgentConfiguration.DefaultConfig)
      .resolve
    val ec = myConfig.getBoolean("js7.pekko.use-js7-thread-pool") ? defaultExecutionContext
    ActorSystem(name, Some(myConfig), Some(Pekkos.getClass.getClassLoader), ec)
      .tap { o =>
        DeadLetterActor.subscribe(o)
        closer.onClose {
          terminateAndWait(o)
        }
      }
  }

}
