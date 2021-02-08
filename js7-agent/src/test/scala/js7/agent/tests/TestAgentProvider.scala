package js7.agent.tests

import com.google.inject.Module
import com.google.inject.util.Modules
import com.google.inject.util.Modules.EMPTY_MODULE
import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.inject.AgentModule
import js7.base.thread.Futures.implicits._
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import monix.execution.Scheduler.Implicits.global

/**
 * @author Joacim Zschimmer
 */
trait TestAgentProvider extends TestAgentDirectoryProvider {

  protected def extraAgentModule: Module = EMPTY_MODULE

  protected lazy val agentConfiguration = newAgentConfiguration()

  protected final def newAgentConfiguration() =
    AgentConfiguration.forTest(configAndData = agentDirectory)

  protected lazy final val agent: RunningAgent = {
    val confModule = new AgentModule(agentConfiguration)
    val combinedModule = Modules.`override`(confModule) `with` extraAgentModule
    RunningAgent.startForTest(combinedModule).map(_.closeWithCloser) await 10.s
  }.withCloser { agent =>
    // Terminate Agent properly to avoid StackOverflowError due to a RejectedExecutionException when terminating Akka 2.6.6
    agent.terminate(Some(SIGKILL)) await 99.s
    agent.close()
  }
}
