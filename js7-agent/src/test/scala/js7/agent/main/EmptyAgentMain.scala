package js7.agent.main

import js7.agent.TestAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.tests.TestAgentDirectoryProvider
import js7.base.thread.MonixBlocking.syntax.RichTask

/** For testing only.
  * @author Joacim Zschimmer
  */
object EmptyAgentMain
{
  def main(args: Array[String]): Unit = {
    TestAgentDirectoryProvider.provideAgentDirectory { directory =>
      val conf = AgentConfiguration.forTest(
        configAndData = directory,
        name = AgentConfiguration.DefaultName,
        httpPort = Some(4445))
      TestAgent.blockingRun(conf) { agent =>
        import agent.scheduler
        agent.untilTerminated.awaitInfinite
      }
    }
  }
}
