package js7.agent.main

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.tests.TestAgentDirectoryProvider
import js7.common.scalautil.Futures.implicits._

/** For testing only.
  * @author Joacim Zschimmer
  */
object EmptyAgentMain
{
  def main(args: Array[String]): Unit = {
    TestAgentDirectoryProvider.provideAgentDirectory { directory =>
      val conf = AgentConfiguration.forTest(configAndData = directory, httpPort = Some(4445))
      RunningAgent.run(conf) { _.terminated.awaitInfinite }
    }
  }
}
