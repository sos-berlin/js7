package com.sos.jobscheduler.agent.test

import org.scalatest.{BeforeAndAfterAll, Suite}

/**
 * @author Joacim Zschimmer
 */
trait AgentTest extends BeforeAndAfterAll with AgentProvider {
  this: Suite ⇒

  override protected def beforeAll() = {
    super.beforeAll()
    agent
  }

  override def afterAll(): Unit = {
    onClose { super.afterAll() }
    close()
  }
}
