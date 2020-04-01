package com.sos.jobscheduler.agent.tests

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, Suite}

/**
 * @author Joacim Zschimmer
 */
trait AgentTester extends BeforeAndAfterAll with TestAgentProvider {
  this: Suite =>

  private var started = false

  override protected def beforeAll() = {
    super.beforeAll()
    agent
    started = true
  }

  override def afterAll() = {
    if (started) {
      agent.terminate() await 99.s
    }
    onClose { super.afterAll() }
    close()
  }
}
