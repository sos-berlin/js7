package com.sos.jobscheduler.agent.test

import com.google.inject.Module
import com.google.inject.util.Modules
import com.google.inject.util.Modules.EMPTY_MODULE
import com.sos.jobscheduler.agent.Agent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.time.ScalaTime._
import org.scalatest.{BeforeAndAfterAll, Suite}

/**
 * @author Joacim Zschimmer
 */
trait AgentTest extends BeforeAndAfterAll {
  this: HasCloser with Suite ⇒

  protected def extraAgentModule: Module = EMPTY_MODULE

  protected def agentConfiguration = AgentConfiguration.forTest()

  protected lazy final val agent = {
    val confModule = new AgentModule(agentConfiguration)
    val combinedModule = Modules.`override`(confModule) `with` extraAgentModule
    new Agent(combinedModule).closeWithCloser
  }

  override protected def beforeAll() = {
    agent.start() await 10.s
    super.beforeAll()
  }
}
