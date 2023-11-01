package js7.agent.tests

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.tests.AgentPekkoNoStackoverflowTest.*
import js7.agent.tests.TestAgentDirectoryProvider.provideAgentDirectory
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import monix.execution.Scheduler.Implicits.traced

/**
  * @author Joacim Zschimmer
  */
final class AgentPekkoNoStackoverflowTest extends OurTestSuite with AgentTester
{
  "Job working directory" in {
    val exception = intercept[RuntimeException] {
      provideAgentDirectory { directory =>
        val conf = AgentConfiguration.forTest(directory, "AgentPekkoNoStackoverflowTest")
        RunningAgent.run(conf, timeout = Some(99.s)) { agent =>
          logger.warn("THROW TEST ERROR")
          sys.error("TEST ERROR")
        }
      }
    }
    assert(exception.getMessage == "TEST ERROR")
  }
}

object AgentPekkoNoStackoverflowTest {
  private val logger = Logger(getClass)
}
