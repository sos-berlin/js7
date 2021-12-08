package js7.agent.configuration

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.utils.Closer
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkautils.Akkas.terminateAndWait
import js7.common.akkautils.DeadLetterActor
import scala.concurrent.ExecutionContext
import scala.util.chaining.scalaUtilChainingOps

/**
 * @author Joacim Zschimmer
 */
object Akkas
{
  def newAgentActorSystem(name: String, config: Config = ConfigFactory.empty, defaultExecutionContext: ExecutionContext = ExecutionContext.global)
    (implicit closer: Closer)
  : ActorSystem = {
    val myConfig = config
      .withFallback(AgentConfiguration.DefaultConfig)
      .resolve
    val ec = myConfig.getBoolean("js7.akka.use-js7-thread-pool") ? defaultExecutionContext
    ActorSystem(name, Some(myConfig), Some(Akkas.getClass.getClassLoader), ec)
      .tap { o =>
        DeadLetterActor.subscribe(o)
        closer.onClose {
          terminateAndWait(o)
        }
      }
  }
}
