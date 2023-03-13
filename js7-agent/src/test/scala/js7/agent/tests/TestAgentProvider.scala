package js7.agent.tests

import js7.agent.configuration.AgentConfiguration
import js7.agent.{RunningAgent, TestAgent}
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.{BeforeAndAfterAll, Suite}

/**
 * @author Joacim Zschimmer
 */
trait TestAgentProvider extends TestAgentDirectoryProvider with BeforeAndAfterAll {
  this: Suite =>

  protected def agentTestWiring: RunningAgent.TestWiring =
    RunningAgent.TestWiring.empty

  protected lazy val agentConfiguration = newAgentConfiguration()

  protected final def newAgentConfiguration() =
    AgentConfiguration.forTest(configAndData = agentDirectory, getClass.simpleScalaName)

  override def afterAll() = {
    // Terminate Agent properly to avoid StackOverflowError due to a RejectedExecutionException when terminating Akka 2.6.6
    agent.terminate(Some(SIGKILL)) await 99.s
    super.afterAll()
  }

  protected lazy final val agent: TestAgent =
    TestAgent
      .start(agentConfiguration, agentTestWiring)
      .await(10.s)
}
