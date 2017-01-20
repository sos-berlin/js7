package com.sos.scheduler.engine.agent.test

import com.google.inject.Module
import com.google.inject.util.Modules
import com.google.inject.util.Modules.EMPTY_MODULE
import com.sos.scheduler.engine.agent.Agent
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.inject.AgentModule
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.time.ScalaTime._
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
