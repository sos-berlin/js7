package com.sos.jobscheduler.tests.https

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.data.MasterCommand
import monix.execution.Scheduler.Implicits.global

/**
  * @author Joacim Zschimmer
  */
private[https] trait MasterHttpsStandardTests extends HttpsTestBase
{
  "overview" in {
    val overview = masterApi.overview await 99.s
    assert(overview.buildId == BuildInfo.buildId)
  }

  "Login" in {
    masterApi.login(Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD"))) await 99.s
  }

  "Run a job" in {
    masterApi.addOrder(FreshOrder(OrderId("TEST"), WorkflowPath("/TEST-WORKFLOW"))) await 99.s
    master.eventWatch.await[OrderFinished]()
  }

  "ShutDown" in {
    masterApi.executeCommand(MasterCommand.ShutDown()) await 99.s
    masterApi.clearSession()  // To avoid automatic logoff because Master is terminating now.
    master.terminated await 99.s
  }
}
