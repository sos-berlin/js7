package js7.agent.tests

import org.scalatest.{BeforeAndAfterAll, Suite}

/**
 * @author Joacim Zschimmer
 */
trait AgentTester extends BeforeAndAfterAll, TestAgentProvider:
  this: Suite =>

  override protected def beforeAll() =
    super.beforeAll()
    agent

  override def afterAll() =
    onClose { super.afterAll() }
    close()
