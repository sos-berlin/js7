package js7.tests

import java.net.ServerSocket
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.data.agent.AgentRefStateEvent.AgentCouplingFailed
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.TerminateWithUnknownAgentTest._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TerminateWithUnknownAgentTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  private lazy val socket = new ServerSocket(0, /*backlog=*/1)
  protected val items = Seq(agentRef, workflow)
  protected val agentPaths = Nil
  override protected def provideAgentClientCertificate = false

  override def afterAll() = {
    socket.close()
    super.afterAll()
  }

  "Terminate Controller while AgentDriver is trying to send a command to a non-existent Agent" in {
    controller.addOrderBlocking(FreshOrder(OrderId("TEST"), workflow.path))
    socket.close()
    eventWatch.await[AgentCouplingFailed]()
    controller.terminate() await 99.s
  }
}

private object TerminateWithUnknownAgentTest
{
  private val agentRef = AgentRef(AgentPath("UNKNOWN"), Uri(s"http://0.0.0.0:0"))
  private val workflow = Workflow.of(WorkflowPath("WORKFLOW"),
    EmptyJob.execute(agentRef.path))
}
