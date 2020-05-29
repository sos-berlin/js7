package com.sos.jobscheduler.tests.master.agent

import com.sos.jobscheduler.agent.data.Problems.DuplicateAgentRef
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.TestExecutablePath
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.{AgentCouplingFailed, AgentReady}
import com.sos.jobscheduler.tests.master.agent.DuplicateAgentRefTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.script
import com.sos.jobscheduler.tests.testenv.MasterAgentForScalaTest
import com.typesafe.config.ConfigUtil.quoteString
import org.scalatest.freespec.AnyFreeSpec

final class DuplicateAgentRefTest extends AnyFreeSpec with MasterAgentForScalaTest
{
  protected val agentRefPaths = aAgentRefPath :: Nil
  protected val fileBased = workflow :: Nil

  import master.scheduler

  override def beforeAll() = {
    (directoryProvider.master.configDir / "private" / "private.conf") ++=
      "jobscheduler.auth.agents." + quoteString(bAgentRefPath.string) + " = " +
        quoteString(directoryProvider.agentToTree(aAgentRefPath).password.string) + "\n"
    for (a <- directoryProvider.agents) a.writeExecutable(TestExecutablePath, script(0.s))
    super.beforeAll()
  }

  "test" in {
    master.eventWatch.await[AgentReady](_.key == aAgentRefPath)
    updateRepo(AgentRef(bAgentRefPath, agent.localUri) :: Nil)

    val orderId = OrderId("ORDER")
    master.addOrderBlocking(FreshOrder(orderId, workflow.path))
    val a = master.eventWatch.await[AgentCouplingFailed]().head.value.event
    val b = AgentCouplingFailed(DuplicateAgentRef(first = aAgentRefPath, second = bAgentRefPath))
    assert(a == b)
  }
}

object DuplicateAgentRefTest
{
  private val aAgentRefPath = AgentRefPath("/A-AGENT")
  private val bAgentRefPath = AgentRefPath("/B-AGENT")
  private val workflow = Workflow.of(
    WorkflowPath("/SINGLE") ~ "INITIAL",
    Execute(WorkflowJob(aAgentRefPath, ExecutablePath("/executable.cmd"))),
    Execute(WorkflowJob(bAgentRefPath, ExecutablePath("/executable.cmd"))))
}
