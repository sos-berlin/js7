package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.job.ExecutableScript
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentCouplingFailed
import com.sos.jobscheduler.tests.TerminateWithUnknownAgentTest._
import com.sos.jobscheduler.tests.testenv.MasterAgentForScalaTest
import java.net.ServerSocket
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TerminateWithUnknownAgentTest extends AnyFreeSpec with MasterAgentForScalaTest
{
  private lazy val socket = new ServerSocket(0, /*backlog=*/1)
  protected val fileBased = workflow :: AgentRef(agentRefPath, Uri(s"http://127.0.0.1:${socket.getLocalPort}")) ::  Nil
  protected val agentRefPaths = Nil
  override protected def agentHttpsMutual = true
  override protected def provideAgentClientCertificate = false

  override def afterAll() = {
    socket.close()
    super.afterAll()
  }

  "Terminate Master while AgentDriver is trying to send a command to a non-existent Agent" in {
    master.addOrderBlocking(FreshOrder(OrderId("TEST"), workflow.path))
    socket.close()
    master.eventWatch.await[AgentCouplingFailed]()
    master.terminate() await 99.s
  }
}

private object TerminateWithUnknownAgentTest
{
  private val agentRefPath = AgentRefPath("/UNKNOWN")
  private val workflow = Workflow.of(WorkflowPath("/WORKFLOW"),
    Execute.Anonymous(WorkflowJob(agentRefPath, ExecutableScript(":"))))
}
