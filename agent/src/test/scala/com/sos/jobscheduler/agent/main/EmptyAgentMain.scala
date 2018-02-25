package com.sos.jobscheduler.agent.main

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.test.TestAgentDirectoryProvider
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import scala.concurrent.ExecutionContext.Implicits.global

/** For testing only.
  * @author Joacim Zschimmer
  */
object EmptyAgentMain
{
  def main(args: Array[String]): Unit = {
    TestAgentDirectoryProvider.provideAgentDirectory { directory ⇒
      val conf = AgentConfiguration.forTest(configAndData = Some(directory), httpPort = 4445)
      RunningAgent.run(conf) { _.terminated.awaitInfinite }
    }
  }
}
