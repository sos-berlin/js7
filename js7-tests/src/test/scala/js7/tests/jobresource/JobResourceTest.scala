package js7.tests.jobresource

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentId
import js7.data.item.CommonItemEvent.ItemAttached
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.job.{JobResource, JobResourceId, ScriptExecutable}
import js7.data.order.OrderEvent.{OrderStdWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.expression.Expression.{ObjectExpression, StringConstant}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobresource.JobResourceTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class JobResourceTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected val agentIds = Seq(agentId)
  protected val versionedItems = Seq(workflow)

  private lazy val signedJobResources = Seq(aJobResource, bJobResource).map(directoryProvider.itemSigner.sign)

  "JobResourceId" in {
    controllerApi.updateSignedSimpleItems(signedJobResources).await(99.s).orThrow
    controller.eventWatch.await[SignedItemAdded](_.event.id == aJobResource.id)
    controller.eventWatch.await[SignedItemAdded](_.event.id == bJobResource.id)

    val orderId = OrderId("ORDER")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    controller.eventWatch.await[ItemAttached](_.event.id == aJobResource.id)
    controller.eventWatch.await[ItemAttached](_.event.id == bJobResource.id)
    controller.eventWatch.await[OrderTerminated](_.key == orderId)
    val stdouterr = controller.eventWatch.keyedEvents[OrderStdWritten](orderId)
      .foldLeft("")(_ + _.chunk)

    assert(stdouterr.replaceAll("\r", "") ==
      """A=/a/
        |B=/bb/
        |C=/ccc/
        |D=/D OF JOB/
        |E=/E OF JOB RESOURCE/
        |""".stripMargin)
  }
}

object JobResourceTest
{
  private val agentId = AgentId("AGENT")

  private val aJobResource = JobResource(
    JobResourceId("JOB-RESOURCE-A"),
    env = ObjectExpression(Map(
      "A" -> StringConstant("a"),
      "B" -> StringConstant("bb"))))

  private val bJobResource = JobResource(
    JobResourceId("JOB-RESOURCE-B"),
    env = ObjectExpression(Map(
      "B" -> StringConstant("IGNORED"),
      "C" -> StringConstant("ccc"),
      "E" -> StringConstant("E OF JOB RESOURCE"))))

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(Execute.Anonymous(
      WorkflowJob(
        agentId,
        ScriptExecutable("""#!/usr/bin/env bash
            |set -euo pipefail
            |echo A=/$A/
            |echo B=/$B/
            |echo C=/$C/
            |echo D=/$D/
            |echo E=/$E/
            |""".stripMargin,
          ObjectExpression(Map(
            "D" -> StringConstant("D OF JOB"),
            "E" -> StringConstant("E OF JOB")))),
        jobResourceIds = Seq(aJobResource.id, bJobResource.id)))))
}
