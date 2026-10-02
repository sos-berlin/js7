package js7.tests.subagent

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.SubagentId
import js7.data.value.expression.Expression.expr
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.subagent.jobs.TestJob
import js7.tests.subagent.SubagentTinyMemoryJournalTest.*

final class SubagentTinyMemoryJournalTest extends OurTestSuite, SubagentTester:

  override protected def agentConfig = config"""
    js7.journal.memory.event-count = 1
    """.withFallback(super.agentConfig)

  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(bareSubagentItem, aJobResource, bJobResource)
  override protected val primarySubagentsDisabled = true

  "Local Subagent" in:
    withItem(
      Workflow(
        WorkflowPath("WORKFLOW"),
        Seq(
          TestJob.execute(agentPath, subagentBundleId = Some(expr"'AGENT-0'"))),
        jobResourcePaths = Seq(aJobResource.path, bJobResource.path))
    ): workflow =>
      val orderId = OrderId("LOCAL")
      runOrder(FreshOrder(orderId, workflow.path))
      assert(controller.eventsByKey[OrderFinished](orderId).size == 1)

  "Remote Subagent" in:
    withItem(
      Workflow(
        WorkflowPath("WORKFLOW"),
        Seq(
          TestJob.execute(agentPath, subagentBundleId = Some(expr"'BARE-SUBAGENT'"))),
        jobResourcePaths = Seq(aJobResource.path, bJobResource.path))
    ): workflow =>
      runSubagent(bareSubagentItem): _ =>
        val orderId = OrderId("REMOTE")
        runOrder(FreshOrder(orderId, workflow.path))
        assert(controller.eventsByKey[OrderFinished](orderId).size == 1)


object SubagentTinyMemoryJournalTest:
  private val localSubagentId = SubagentId("AGENT-0")
  private val aJobResource = JobResource(JobResourcePath("A-JOB-RESOURCE"))
  private val bJobResource = JobResource(JobResourcePath("B-JOB-RESOURCE"))
