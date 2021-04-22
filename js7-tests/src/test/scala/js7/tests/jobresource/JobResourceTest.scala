package js7.tests.jobresource

import cats.implicits._
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.job.{JobResource, JobResourcePath, ScriptExecutable}
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.StringValue
import js7.data.value.expression.Expression.{ObjectExpression, StringConstant}
import js7.data.value.expression.ExpressionParser
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
  protected val versionedItems = Seq(workflow, envWorkflow, sosWorkflow)

  "JobResourcePath" in {
    controllerApi.updateSignedSimpleItems(Seq(aJobResource, bJobResource) map sign)
      .await(99.s).orThrow
    controller.eventWatch.await[SignedItemAdded](_.event.id == aJobResource.id)
    controller.eventWatch.await[SignedItemAdded](_.event.id == bJobResource.id)

    val orderId = OrderId("ORDER")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path, Map(
      "A" -> StringValue("A OF ORDER")
    ))).await(99.s).orThrow
    controller.eventWatch.await[ItemAttached](_.event.id == aJobResource.id)
    controller.eventWatch.await[ItemAttached](_.event.id == bJobResource.id)
    controller.eventWatch.await[OrderTerminated](_.key == orderId)

    val stdouterr = controller.eventWatch.keyedEvents[OrderStdWritten](orderId).foldMap(_.chunk)
    assert(stdouterr.replaceAll("\r", "") ==
      """A=/A of JOB-RESOURCE-A/
        |B=/B of JOB-RESOURCE-A/
        |C=/C of JOB-RESOURCE-B/
        |D=/D OF JOB ENV/
        |E=/E OF JOB-RESOURCE-B/
        |""".stripMargin)
  }

  "Change JobResourcePath" in {
    val eventId = controller.eventWatch.lastAddedEventId
    controllerApi.updateSignedSimpleItems(Seq(sign(b1JobResource))).await(99.s).orThrow
    val orderId = OrderId("ORDER-1")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    controller.eventWatch.await[ItemAttached](_.event.id == b1JobResource.id, after = eventId)
  }

  "JobResourcePath with variable references (there are no variables)" in {
    val eventId = controller.eventWatch.lastAddedEventId
    controllerApi.updateSignedSimpleItems(Seq(sign(b2JobResource))).await(99.s).orThrow
    controller.eventWatch.await[ItemAttached](_.event.id == b2JobResource.id, after = eventId)

    val orderId = OrderId("ORDER-2")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path, Map(
      "E" -> StringValue("E OF ORDER")
    ))).await(99.s).orThrow
    controller.eventWatch.await[OrderTerminated](_.key == orderId)

    // JobResource must not use order variables
    val orderProcessed = controller.eventWatch.await[OrderProcessed](_.key == orderId).head.value.event
    assert(orderProcessed.outcome == Outcome.Failed(Some("No such named value: E")))
  }

  "JobResourcePath with environment variable access" in {
    controllerApi.updateSignedSimpleItems(Seq(sign(envJobResource))).await(99.s).orThrow

    val orderId = OrderId("ORDER-ENV")
    controllerApi.addOrder(FreshOrder(orderId, envWorkflow.path)).await(99.s).orThrow
    controller.eventWatch.await[OrderTerminated](_.key == orderId)

    val stdouterr = controller.eventWatch.keyedEvents[OrderStdWritten](orderId).foldMap(_.chunk)
    assert(stdouterr.replaceAll("\r", "") ==
      s"""ENV=/${sys.env(envName)}/
         |""".stripMargin)
  }

  "Example for an SOS JobResource" in {
    controllerApi.updateSignedSimpleItems(Seq(sign(sosJobResource))).await(99.s).orThrow

    val orderId = OrderId("ORDER-SOS")
    controllerApi.addOrder(FreshOrder(orderId, sosWorkflow.path)).await(99.s).orThrow
    controller.eventWatch.await[OrderTerminated](_.key == orderId)
    assert(controller.eventWatch.await[OrderProcessed](_.key == orderId).head.value.event.outcome ==
      Outcome.succeededRC0)

    val stdouterr = controller.eventWatch.keyedEvents[OrderStdWritten](orderId).foldMap(_.chunk)
    scribe.info(stdouterr.trim)
  }
}

object JobResourceTest
{
  private val agentId = AgentPath("AGENT")

  private val aJobResource = JobResource(
    JobResourcePath("JOB-RESOURCE-A"),
    env = ObjectExpression(Map(
      "A" -> StringConstant("A of JOB-RESOURCE-A"),
      "B" -> StringConstant("B of JOB-RESOURCE-A"))))

  private val bJobResource = JobResource(
    JobResourcePath("JOB-RESOURCE-B"),
    env = ObjectExpression(Map(
      "B" -> StringConstant("IGNORED"),
      "C" -> StringConstant("C of JOB-RESOURCE-B"),
      "E" -> StringConstant("E OF JOB-RESOURCE-B"))))

  private val b1JobResource = JobResource(JobResourcePath("JOB-RESOURCE-B"))

  private val b2JobResource = JobResource(
    JobResourcePath("JOB-RESOURCE-B"),
    env = ObjectExpression(Map(
      "B" -> StringConstant("IGNORED"),
      "E" -> ExpressionParser.parse(""""E=$E"""").orThrow)))

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(Execute.Anonymous(
      WorkflowJob(
        agentId,
        ScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |echo A=/$A/
            |echo B=/$B/
            |echo C=/$C/
            |echo D=/$D/
            |echo E=/$E/
            |""".stripMargin,
          env = ObjectExpression(Map(
            "D" -> StringConstant("D OF JOB ENV"),
            "E" -> StringConstant("E OF JOB ENV")))),
        defaultArguments = Map("A" -> StringValue("A of WorkflowJob")),
        jobResourcePaths = Seq(aJobResource.id, bJobResource.id)),
      defaultArguments = Map("A" -> StringValue("A of Execute")))))

  private val envName = if (isWindows) "Path" else "PATH"
  private val envJobResource = JobResource(
    JobResourcePath("JOB-RESOURCE-ENV"),
    env = ObjectExpression(Map(
      "ENV" -> ExpressionParser.parse(s"env('$envName')").orThrow)))

  private val envWorkflow = Workflow(
    WorkflowPath("WORKFLOW-ENV") ~ "INITIAL",
    Vector(Execute.Anonymous(
      WorkflowJob(
        agentId,
        ScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |echo ENV=/$ENV/
            |""".stripMargin),
        jobResourcePaths = Seq(envJobResource.id)))))

  private val sosJobResource = JobResource(
    JobResourcePath("JOB-RESOURCE-SPS"),
    env = ObjectExpression(Map(
      "JS7_CONTROLLER_ID"     -> ExpressionParser.parse("$js7ControllerId").orThrow,
      "JS7_WORKFLOW_NAME"     -> ExpressionParser.parse("$js7WorkflowPath").orThrow,
      "JS7_WORKFLOW_POSITION" -> ExpressionParser.parse("$js7WorkflowPosition").orThrow,
      "JS7_ORDER_ID"          -> ExpressionParser.parse("$js7OrderId").orThrow,
      "JS7_JOB_NAME"          -> ExpressionParser.parse("$js7JobName").orThrow,
      //"JS7_SCHEDULED_DATE"    -> ExpressionParser.parse("now(format='yyyy-MM-dd')").orThrow,
      //"JS7_SCHEDULED_DAY"     -> ExpressionParser.parse("now(format='dd')").orThrow,
      //"JS7_SCHEDULED_MONTH"   -> ExpressionParser.parse("now(format='MM')").orThrow,
      //"JS7_SCHEDULED_YEAR"    -> ExpressionParser.parse("now(format='yyyy')").orThrow,
      //"JS7_SCHEDULED_HOUR"    -> ExpressionParser.parse("now(format='HH')").orThrow,
      //"JS7_SCHEDULED_MINUTE"  -> ExpressionParser.parse("now(format='mm')").orThrow,
      //"JS7_SCHEDULED_SECOND"  -> ExpressionParser.parse("now(format='SS')").orThrow,
      "JS7_TASKSTART_DATE"    -> ExpressionParser.parse("now(format='yyyy-MM-dd')").orThrow,
      "JS7_TASKSTART_DAY"     -> ExpressionParser.parse("now(format='dd')").orThrow,
      "JS7_TASKSTART_MONTH"   -> ExpressionParser.parse("now(format='MM')").orThrow,
      "JS7_TASKSTART_YEAR"    -> ExpressionParser.parse("now(format='yyyy')").orThrow,
      "JS7_TASKSTART_HOUR"    -> ExpressionParser.parse("now(format='HH')").orThrow,
      "JS7_TASKSTART_MINUTE"  -> ExpressionParser.parse("now(format='mm')").orThrow,
      "JS7_TASKSTART_SECOND"  -> ExpressionParser.parse("now(format='SS')").orThrow)))

  private val sosWorkflow = Workflow(
    WorkflowPath("WORKFLOW-SOS") ~ "INITIAL",
    Vector(Execute.Anonymous(
      WorkflowJob(
        agentId,
        ScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |echo JS7_CONTROLLER_ID=/$JS7_CONTROLLER_ID/
            |echo JS7_WORKFLOW_NAME=/$JS7_WORKFLOW_NAME/
            |echo JS7_WORKFLOW_POSITION=/$JS7_WORKFLOW_POSITION/
            |echo JS7_ORDER_ID=/$JS7_ORDER_ID/
            |echo JS7_JOB_NAME=/$JS7_JOB_NAME/
            |echo JS7_TASKSTART_DATE=/$JS7_TASKSTART_DATE/
            |echo JS7_TASKSTART_DAY=/$JS7_TASKSTART_DAY/
            |echo JS7_TASKSTART_MONTH=/$JS7_TASKSTART_MONTH/
            |echo JS7_TASKSTART_YEAR=/$JS7_TASKSTART_YEAR/
            |echo JS7_TASKSTART_HOUR=/$JS7_TASKSTART_HOUR/
            |echo JS7_TASKSTART_MINUTE=/$JS7_TASKSTART_MINUTE/
            |echo JS7_TASKSTART_SECOND=/$JS7_TASKSTART_SECOND/
            |""".stripMargin),
        //echo JS7_SCHEDULED_DATE=/$JS7_SCHEDULED_DATE/
        //echo JS7_SCHEDULED_DAY=/$JS7_SCHEDULED_DAY/
        //echo JS7_SCHEDULED_MONTH=/$JS7_SCHEDULED_MONTH/
        //echo JS7_SCHEDULED_YEAR=/$JS7_SCHEDULED_YEAR/
        //echo JS7_SCHEDULED_HOUR=/$JS7_SCHEDULED_HOUR/
        //echo JS7_SCHEDULED_MINUTE=/$JS7_SCHEDULED_MINUTE/
        //echo JS7_SCHEDULED_SECOND=/$JS7_SCHEDULED_SECOND/
        jobResourcePaths = Seq(sosJobResource.id)))))
}
