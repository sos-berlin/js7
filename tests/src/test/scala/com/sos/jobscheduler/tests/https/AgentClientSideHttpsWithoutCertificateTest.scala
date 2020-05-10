package com.sos.jobscheduler.tests.https

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentCouplingFailed
import com.sos.jobscheduler.tests.https.AgentClientSideHttpsWithoutCertificateTest._
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
    masterApi.login() await 99.s
  }

  "Run a job" in {
    masterApi.addOrder(FreshOrder(OrderId("TEST"), WorkflowPath("/TEST-WORKFLOW"))) await 99.s
    val KeyedEvent(AgentRefPath("/TEST-AGENT"), AgentCouplingFailed(problem)) = master.eventWatch.await[AgentCouplingFailed](timeout = 99.seconds).head.value
    logger.info(problem.toString)  // Content of exception is not reliable. May be SSLxxException or TCP connection reset !!!
    //assert(msg == "javax.net.ssl.SSLException: Received fatal alert: certificate_unknown" ||
    //       msg.startsWith("javax.net.ssl.SSLHandshakeException:") ||  // Since Java 11
    //       msg == "akka.stream.StreamTcpException: The connection closed with error: Connection reset by peer")
  }
}

private object AgentClientSideHttpsWithoutCertificateTest
{
  private val logger = Logger(getClass)
}
