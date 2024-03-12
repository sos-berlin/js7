package js7.tests

import java.net.ServerSocket
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.data.agent.AgentRefStateEvent.AgentCouplingFailed
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.TerminateWithUnknownAgentTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest

/**
  * @author Joacim Zschimmer
  */
final class TerminateWithUnknownAgentTest extends OurTestSuite, ControllerAgentForScalaTest:

  private lazy val socket = new ServerSocket(0, /*backlog=*/1)
  protected val items = Seq(agentRef, workflow)
  protected val agentPaths = Nil
  override protected def provideAgentClientCertificate = false

  override def afterAll() =
    socket.close()
    super.afterAll()

  "Terminate Controller while AgentDriver is trying to send a command to a non-existent Agent" in:
    controller.addOrderBlocking(FreshOrder(OrderId("TEST"), workflow.path))
    socket.close()
    eventWatch.await[AgentCouplingFailed]()
    controller.terminate() await 99.s


private object TerminateWithUnknownAgentTest:
  private val agentRef = AgentRef(AgentPath("UNKNOWN"), directors = Nil,
    uri = Some(Uri("http://0.0.0.0:0")))
  private val workflow = Workflow.of(WorkflowPath("WORKFLOW"),
    EmptyJob.execute(agentRef.path))
