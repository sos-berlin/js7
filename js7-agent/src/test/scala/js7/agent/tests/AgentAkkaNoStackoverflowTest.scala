package js7.agent.tests

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.tests.AgentAkkaNoStackoverflowTest._
import js7.agent.tests.TestAgentDirectoryProvider.provideAgentDirectory
import js7.base.log.Logger
import js7.base.time.ScalaTime._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentAkkaNoStackoverflowTest extends AnyFreeSpec with AgentTester
{
  "Job working directory" in {
    val exception = intercept[RuntimeException] {
      provideAgentDirectory { directory =>
        val conf = AgentConfiguration.forTest(directory, "AgentAkkaNoStackoverflowTest")
        RunningAgent.run(conf, timeout = Some(99.s)) { agent =>
          logger.warn("THROW TEST ERROR")
          sys.error("TEST ERROR")
        }
      }
    }
    assert(exception.getMessage == "TEST ERROR")
  }
}

object AgentAkkaNoStackoverflowTest {
  private val logger = Logger(getClass)
}
