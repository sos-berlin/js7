package js7.tests

import java.net.ServerSocket
import js7.base.problem.Checked._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.data.agent.AgentRefStateEvent.AgentCouplingFailed
import js7.data.agent.{AgentId, AgentRef}
import js7.data.job.ScriptExecutable
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.TerminateWithUnknownAgentTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TerminateWithUnknownAgentTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  private lazy val socket = new ServerSocket(0, /*backlog=*/1)
  protected val versionedItems = workflow ::  Nil
  protected val agentIds = Nil
  override protected def provideAgentClientCertificate = false

  override def afterAll() = {
    socket.close()
    super.afterAll()
  }

  "Terminate Controller while AgentDriver is trying to send a command to a non-existent Agent" in {
    controller.updateUnsignedSimpleItemsAsSystemUser(Seq(AgentRef(agentId, Uri(s"http://127.0.0.1:${socket.getLocalPort}"))))
      .await(99.s).orThrow
    controller.addOrderBlocking(FreshOrder(OrderId("TEST"), workflow.path))
    socket.close()
    controller.eventWatch.await[AgentCouplingFailed]()
    controller.terminate() await 99.s
  }
}

private object TerminateWithUnknownAgentTest
{
  private val agentId = AgentId("UNKNOWN")
  private val workflow = Workflow.of(WorkflowPath("WORKFLOW"),
    Execute.Anonymous(WorkflowJob(agentId, ScriptExecutable(":"))))
}
