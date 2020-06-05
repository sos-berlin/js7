package js7.tests.https

import js7.base.time.ScalaTime._
import js7.common.scalautil.Logger
import js7.common.scalautil.MonixUtils.syntax._
import js7.data.agent.AgentRefPath
import js7.data.event.KeyedEvent
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.WorkflowPath
import js7.master.data.events.MasterAgentEvent.AgentCouplingFailed
import js7.tests.https.AgentClientSideHttpsWithoutCertificateTest._
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
