package com.sos.jobscheduler.tests.https

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.KeyedEvent
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
    masterApi.login(Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD"))) await 99.s
  }

  "Run a job" in {
    masterApi.addOrder(FreshOrder(OrderId("TEST"), WorkflowPath("/TEST-WORKFLOW"))) await 99.s
    val KeyedEvent(AgentRefPath("/TEST-AGENT"), AgentCouplingFailed(msg)) = master.eventWatch.await[AgentCouplingFailed](timeout = 99.seconds).head.value
    assert(msg == "javax.net.ssl.SSLException: Received fatal alert: certificate_unknown" ||
           msg == "akka.stream.StreamTcpException: The connection closed with error: Connection reset by peer")
  }
}
