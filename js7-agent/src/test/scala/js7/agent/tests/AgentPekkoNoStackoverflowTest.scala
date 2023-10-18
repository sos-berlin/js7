package js7.agent.tests

import js7.agent.TestAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.tests.AgentPekkoNoStackoverflowTest.*
import js7.agent.tests.TestAgentDirectoryProvider.provideAgentDirectory
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*

/**
  * @author Joacim Zschimmer
  */
final class AgentPekkoNoStackoverflowTest extends OurTestSuite with AgentTester:
  "Job working directory" in:
    val exception = intercept[RuntimeException]:
      provideAgentDirectory { directory =>
        val conf = AgentConfiguration.forTest(directory, "AgentPekkoNoStackoverflowTest")
        TestAgent.blockingRun(conf, 99.s) { _ =>
          logger.warn("THROW TEST ERROR")
          sys.error("TEST ERROR")
        }
      }
    assert(exception.getMessage == "TEST ERROR")


object AgentPekkoNoStackoverflowTest:
  private val logger = Logger[this.type]
