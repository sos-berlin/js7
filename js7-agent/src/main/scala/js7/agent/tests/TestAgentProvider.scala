package js7.agent.tests

import com.google.inject.Module
import com.google.inject.util.Modules
import com.google.inject.util.Modules.EMPTY_MODULE
import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.inject.AgentModule
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.common.scalautil.Futures.implicits._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author Joacim Zschimmer
 */
trait TestAgentProvider extends TestAgentDirectoryProvider {

  protected def extraAgentModule: Module = EMPTY_MODULE

  protected lazy val agentConfiguration = newAgentConfiguration()

  protected final def newAgentConfiguration() = AgentConfiguration.forTest(configAndData = agentDirectory)

  protected lazy final val agent: RunningAgent = {
    val confModule = new AgentModule(agentConfiguration)
    val combinedModule = Modules.`override`(confModule) `with` extraAgentModule
    RunningAgent.startForTest(combinedModule) map { _.closeWithCloser } await 10.s
  }.closeWithCloser
}
