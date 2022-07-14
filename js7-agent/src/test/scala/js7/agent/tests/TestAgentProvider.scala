package js7.agent.tests

import com.google.inject.Module
import com.google.inject.util.Modules
import com.google.inject.util.Modules.EMPTY_MODULE
import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.inject.AgentModule
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Closer.syntax.*
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import monix.execution.Scheduler.Implicits.traced

/**
 * @author Joacim Zschimmer
 */
trait TestAgentProvider extends TestAgentDirectoryProvider {

  protected def extraAgentModule: Module = EMPTY_MODULE

  protected lazy val agentConfiguration = newAgentConfiguration()

  protected final def newAgentConfiguration() =
    AgentConfiguration.forTest(configAndData = agentDirectory, getClass.simpleScalaName)

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
