package com.sos.jobscheduler.tests.https

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentCouplingFailed
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class AgentClientSideHttpsWithoutCertificateTest extends HttpsTestBase
{
  override protected def agentHttpsMutual = true

  override protected def provideAgentClientCertificate = false

  "Login" in {
    masterApi.login(Some(UserId("TEST-USER") → SecretString("TEST-PASSWORD"))) await 99.s
  }

  "Run a job" in {
    masterApi.addOrder(FreshOrder(OrderId("TEST"), WorkflowPath("/TEST-WORKFLOW"))) await 99.s
    assert(eventCollector.await[AgentCouplingFailed](timeout = 99.seconds)
      .head.value == (AgentPath("/TEST-AGENT") <-: AgentCouplingFailed("javax.net.ssl.SSLException: Received fatal alert: certificate_unknown")))
  }
}
