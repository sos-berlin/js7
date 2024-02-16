package js7.tests.jobresource

import cats.implicits.*
import io.circe.syntax.EncoderOps
import java.lang.System.lineSeparator as nl
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.system.OperatingSystem.{PathEnvName, isMac, isWindows}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.JavaTimestamp.specific.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.MissingReferencedItemProblem
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.{CancelOrders, DeleteOrdersWhenTerminated}
import js7.data.item.BasicItemEvent.{ItemAttached, ItemDeleted}
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion, DeleteSimple}
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.{ItemOperation, VersionId}
import js7.data.job.{JobResource, JobResourcePath, ShellScriptExecutable}
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed, OrderStdWritten, OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.StringValue
import js7.data.value.ValueType.UnknownNameInExpressionProblem
import js7.data.value.expression.Expression.{NamedValue, StringConstant}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.jobresource.JobResourceTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import org.scalatest.Assertions.*

class JobResourceTest extends OurTestSuite, ControllerAgentForScalaTest
{
  override protected final val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, envWorkflow, sosWorkflow, internalWorkflow,
    aJobResource, bJobResource, envJobResource, sosJobResource)

  "referencedItemPaths" in {
    assert(workflow.referencedItemPaths.toSet ==
      Set(aJobResource.path, bJobResource.path, agentPath))
  }

  "JobResource" in {
    controller.eventWatch.await[SignedItemAdded](_.event.key == aJobResource.path)
    controller.eventWatch.await[SignedItemAdded](_.event.key == bJobResource.path)

    val orderId = OrderId("ORDER")
    controller.api.addOrder(FreshOrder(orderId, workflow.path, Map(
      "A" -> StringValue("A of ORDER")
    ))).await(99.s).orThrow
    controller.eventWatch.await[ItemAttached](_.event.key == aJobResource.path)
    controller.eventWatch.await[ItemAttached](_.event.key == bJobResource.path)
    val terminated = controller.eventWatch.await[OrderTerminated](_.key == orderId).head
    assert(terminated.value.event.isInstanceOf[OrderFinished])

    val stdouterr = controller.eventWatch.eventsByKey[OrderStdWritten](orderId).foldMap(_.chunk)
    assert(stdouterr.replaceAll("\r", "") ==
      """A=/A of JOB-RESOURCE-A/
        |B=/B of JOB-RESOURCE-A/
        |C=/C of JOB-RESOURCE-B/
        |D=/D of JOB ENV/
        |E=/E of JOB-RESOURCE-B/
        |aJobResourceVariable=/A of JOB-RESOURCE-A/
        |""".stripMargin)
  }

  "Change JobResource" in {
    val eventId = controller.eventWatch.lastAddedEventId
    controller.api.updateSignedItems(Seq(sign(b1JobResource))).await(99.s).orThrow
    val orderId = OrderId("ORDER-1")
    controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    controller.eventWatch.await[ItemAttached](_.event.key == b1JobResource.path, after = eventId)
  }

  "JobResource with order variable references (no order access)" in {
    val eventId = controller.eventWatch.lastAddedEventId
    controller.api.updateSignedItems(Seq(sign(b2JobResource))).await(99.s).orThrow
    controller.eventWatch.await[ItemAttached](_.event.key == b2JobResource.path, after = eventId)

    val orderId = OrderId("ORDER-2")
    val events = controller.runOrder(FreshOrder(orderId, workflow.path, Map(
      "E" -> StringValue("E of ORDER"))))
    events.map(_.value).collect { case OrderStdWritten(outerr, chunk) => logger.debug(s"$outerr: $chunk") }

    // JobResource must not use order variables
    val orderProcessed = controller.eventWatch.await[OrderProcessed](_.key == orderId).head.value.event
    assert(orderProcessed.outcome == Outcome.Failed(Some("No such named value: E")))
  }

  "JobResource with environment variable access" in {
    val orderId = OrderId("ORDER-ENV")
    controller.api.addOrder(FreshOrder(orderId, envWorkflow.path)).await(99.s).orThrow
    val terminated = controller.eventWatch.await[OrderTerminated](_.key == orderId).head
    assert(terminated.value.event.isInstanceOf[OrderFinished])

    val stdouterr = controller.eventWatch.eventsByKey[OrderStdWritten](orderId).foldMap(_.chunk)
    assert(stdouterr.replaceAll("\r", "") ==
      s"""ORIGINAL_PATH=/${sys.env(PathEnvName)}/
         |""".stripMargin)
  }

  "Example for an SOS JobResource" - {
    "without scheduledFor" in {
      val orderId = OrderId("ORDER-SOS")
      controller.api.addOrder(FreshOrder(orderId, sosWorkflow.path)).await(99.s).orThrow
      val terminated = controller.eventWatch.await[OrderTerminated](_.key == orderId).head
      assert(terminated.value.event.isInstanceOf[OrderFinished])

      assert(controller.eventWatch.await[OrderProcessed](_.key == orderId).head.value.event.outcome ==
        Outcome.succeededRC0)

      val stdouterr = controller.eventWatch.eventsByKey[OrderStdWritten](orderId).foldMap(_.chunk)
      logger.info(stdouterr.trim)
      assert(stdouterr contains s"JS7_ORDER_ID=/ORDER-SOS/$nl")
      assert(stdouterr contains s"JS7_WORKFLOW_NAME=/WORKFLOW-SOS/$nl")
      assert(stdouterr contains s"JS7_JOB_NAME=/TEST-JOB/$nl")
      assert(stdouterr contains s"JS7_LABEL=/TEST-LABEL/$nl")
      assert(stdouterr contains s"JS7_SCHEDULED_DATE=//$nl")
      assert(stdouterr contains s"JS7_SCHEDULED_YEAR=//$nl")
      assert(stdouterr contains s"JS7_SCHEDULED_MONTH=//$nl")
      assert(stdouterr contains s"JS7_SCHEDULED_DAY=//$nl")
      assert(stdouterr contains s"JS7_SCHEDULED_HOUR=//$nl")
      assert(stdouterr contains s"JS7_SCHEDULED_MINUTE=//$nl")
      assert(stdouterr contains s"JS7_SCHEDULED_SECOND=//$nl")
    }

    "with scheduledFor" in {
      controller.api.updateSignedItems(Seq(sign(sosJobResource))).await(99.s).orThrow

      val orderId = OrderId("ORDER-SOS-SCHEDULED")
      val scheduledFor = Timestamp("2021-04-26T00:11:22.789Z")
      controller.api.addOrder(FreshOrder(orderId, sosWorkflow.path, scheduledFor = Some(scheduledFor)))
        .await(99.s).orThrow
      val terminated = controller.eventWatch.await[OrderTerminated](_.key == orderId).head
      assert(terminated.value.event.isInstanceOf[OrderFinished])

      assert(controller.eventWatch.await[OrderProcessed](_.key == orderId).head.value.event.outcome ==
        Outcome.succeededRC0)

      val stdouterr = controller.eventWatch.eventsByKey[OrderStdWritten](orderId).foldMap(_.chunk)
      logger.info(stdouterr.trim)
      val dateTime = scheduledFor
        .toOffsetDateTime(ZoneId.systemDefault())
        .format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ssZ"))
      assert(stdouterr contains s"JS7_SCHEDULED_DATE=/$dateTime/$nl")
      assert(stdouterr contains s"JS7_SCHEDULED_YEAR=/2021/$nl")
      assert(stdouterr contains s"JS7_SCHEDULED_MINUTE=/11/$nl")
      assert(stdouterr contains s"JS7_SCHEDULED_SECOND=/22/$nl")
    }
  }

  "JobResource.variables in JVM job" in {
    val orderId = OrderId("ORDER-INTERNAL")
    val events = controller.runOrder(FreshOrder(orderId, internalWorkflow.path))
    assert(events.last.value.isInstanceOf[OrderFinished])
  }

  "Order fails when referencing an unknown JobResource in an expression" in {
    val workflow = Workflow(
      WorkflowPath("INVALID-WORKFLOW") ~ "INVALID",
      Vector(
        Execute.Anonymous(WorkflowJob(
          agentPath,
          ShellScriptExecutable(":",
            env = Map(
              "aJobResourceVariable" -> expr("JobResource:UNKNOWN:a")))))))
    assert(controller.api.updateSignedItems(Seq(sign(workflow)), Some(workflow.id.versionId))
      .await(99.s) == Left(MissingReferencedItemProblem(workflow.id, JobResourcePath("UNKNOWN"))))
  }

  "Accessing an missing JobResource variable" - {
    val existingName = if isWindows then "TEMP" else if isMac then "TMPDIR" else "HOSTNAME"
    val existingValue = sys.env(existingName)

    "Order fails" in {
      val workflow = addUnknownJobResourceVariableWorkflow("MISSING", "JobResource:JOB-RESOURCE-A:UNKNOWN")
      val events = controller.runOrder(FreshOrder(OrderId("UNKNOWN"), workflow.path))
      assert(events.map(_.value).contains(
        OrderProcessed(Outcome.Disrupted(
          UnknownNameInExpressionProblem("JobResource:JOB-RESOURCE-A:UNKNOWN")))))
    }

    //"Environment variable is left unchanged when the ? operator is used" in {
    //  --> Not true anymore: An existent environment variable is being deleted
    //  assert(existingValue != "", s"Expecting the $existingName environment variable")
    //  val workflow = addUnknownJobResourceVariableWorkflow("NONE", "JobResource:JOB-RESOURCE-A:UNKNOWN ?")
    //  val events = controller.runOrder(FreshOrder(OrderId("UNKNOWN-2"), workflow.path))
    //  assert(events.map(_.value) contains OrderProcessed(Outcome.succeededRC0))
    //  val stdout = events.map(_.value).collect { case OrderStdoutWritten(chunk) => chunk }.mkString
    //  assert(stdout contains s"$existingName=/$existingValue/")
    //}

    def addUnknownJobResourceVariableWorkflow(name: String, exprString: String) = {
      val workflow = Workflow(
        WorkflowPath(name) ~ name,
        Vector(
          Execute.Anonymous(WorkflowJob(
            agentPath,
            ShellScriptExecutable(
              if isWindows then s"@echo off\r\necho $existingName=/%$existingName%/\r\n"
              else s"echo $existingName=/$$$existingName/",
              env = Map(
                existingName -> expr(exprString)))))))
      controller.api.updateSignedItems(Seq(sign(workflow)), Some(workflow.id.versionId))
        .await(99.s).orThrow
      workflow
    }
  }

  "Delete a JobResource" in {
    val deleteJobResource =
      controller.api.updateItems(Stream.emit(DeleteSimple(bJobResource.path)))

    // ItemIsStillReferenced
    assert(deleteJobResource.await(99.s)
      .left.toOption
      .map(_.toString)  // Problem objects are lost at HTTP transfer
      .exists(_.contains("ItemIsStillReferenced")))

    // Cancel and delete Orders
    val referencingWorkflows = Set(workflow.id, internalWorkflow.id)
    val orderIds = controller.api
      .controllerState.await(99.s).orThrow
      .idToOrder.values
      .filter(o => referencingWorkflows(o.workflowId))
      .map(_.id)
      .toSeq
    controller.api
      .executeCommand(CancelOrders(orderIds, mode = CancellationMode.kill(immediately = true)))
      .await(99.s).orThrow
    controller.api.executeCommand(DeleteOrdersWhenTerminated(orderIds))
      .await(99.s).orThrow

    // Remove workflows
    controller.api
      .updateItems(Stream(
        AddVersion(VersionId("JOBRESOURCE-DELETED")),
        ItemOperation.RemoveVersioned(workflow.path),
        ItemOperation.RemoveVersioned(internalWorkflow.path)))
      .await(99.s).orThrow

    eventWatch.await[ItemDeleted](_.event.key == workflow.id)
    eventWatch.await[ItemDeleted](_.event.key == internalWorkflow.id)

    // Now, the JobResource is deletable
    deleteJobResource.await(99.s).orThrow

    // JobResource can be added again
    val v = VersionId("JOBRESOURCE-ADDED-AGAIN")
    controller.api
      .updateItems(Stream(
        AddVersion(v),
        AddOrChangeSigned(toSignedString(bJobResource)),
        AddOrChangeSigned(toSignedString(workflow.withId(workflow.path ~ v)))))
      .await(99.s).orThrow
  }

  "toFile" in {
    // Test is slow due to >20MB stdout (many OrderStdoutWritten events)

    val resourceContent = (1 to 1_000_000).view.map(i => s"RESOURCE-$i\n").mkString
    assert(resourceContent.length >= 10_000_000)

    val executableContent = (1 to 1_000_000).view.map(i => s"EXECUTABLE-$i\n").mkString
    assert(executableContent.length >= 10_000_000)

    val myJobResource = JobResource(
      JobResourcePath("JOB-RESOURCE-FILE"),
      variables = Map(
        "file" -> expr(s"""toFile(${StringConstant.quote(resourceContent)}, "*.tmp")""")),
      env = Map(
        "FROMRESOURCE" -> expr("$file")))

    val versionId = VersionId("toFile")
    val myWorkflow = Workflow(
      WorkflowPath("WORKFLOW-FILE") ~ versionId,
      Vector(Execute.Anonymous(
        WorkflowJob(
          agentPath,
          ShellScriptExecutable(
            if isWindows then
              """@echo off
                |echo FROMRESOURCE=/%FROMRESOURCE%/
                |type %FROMRESOURCE%
                |echo FROMEXECUTABLE=/%FROMEXECUTABLE%/
                |type %FROMEXECUTABLE%
                |""".stripMargin
            else
              """#!/usr/bin/env bash
                 |set -euo pipefail
                 |echo FROMRESOURCE=/$FROMRESOURCE/
                 |cat "$FROMRESOURCE"
                 |echo FROMEXECUTABLE=/$FROMEXECUTABLE/
                 |cat "$FROMEXECUTABLE"
                 |""".stripMargin,
            env = Map(
              "FROMEXECUTABLE" -> expr(s"""toFile(${StringConstant.quote(executableContent)}, "*.tmp")"""))),
          jobResourcePaths = Seq(myJobResource.path)))))

    controller.api.updateSignedItems(Seq(sign(myJobResource), sign(myWorkflow)), Some(versionId))
      .await(99.s).orThrow

    val orderId = OrderId("TMPFILE")
    val events = controller.runOrder(FreshOrder(orderId, myWorkflow.path)).map(_.value)
    assert(events.last == OrderFinished())
    val stdout = events
      .collect { case OrderStdoutWritten(chunk) => chunk }
      .combineAll
    assert(stdout contains resourceContent)
    assert(stdout contains executableContent)
  }
}


object JobResourceTest
{
  private val logger = Logger[this.type]
  private[jobresource] val agentPath = AgentPath("AGENT")

  private val aJobResource = JobResource(
    JobResourcePath("JOB-RESOURCE-A"),
    variables = Map(
      "a" -> StringConstant("A of JOB-RESOURCE-A"),
      "b" -> StringConstant("B of JOB-RESOURCE-A")),
    env = Map(
      "A" -> NamedValue("a"),
      "B" -> NamedValue("b")))

  private val bJobResource = JobResource(
    JobResourcePath("JOB-RESOURCE-B"),
    variables = Map(
      "b" -> StringConstant("ignored"),
      "c" -> StringConstant("C of JOB-RESOURCE-B"),
      "e" -> StringConstant("E of JOB-RESOURCE-B")),
    env = Map(
      "B" -> StringConstant("IGNORED"),
      "C" -> StringConstant("C of JOB-RESOURCE-B"),
      "E" -> StringConstant("E of JOB-RESOURCE-B")))

  private val b1JobResource = JobResource(JobResourcePath("JOB-RESOURCE-B"))

  private val b2JobResource = JobResource(
    JobResourcePath("JOB-RESOURCE-B"),
    env = Map(
      "B" -> StringConstant("IGNORED IN FAVOR OF JOB-RESOURCE-A"),
      "E" -> expr(""""E=$E"""")))

  private val jobName = WorkflowJob.Name("TEST-JOB")
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      Execute.Named(jobName, defaultArguments = Map("A" -> StringConstant("A of Execute")))),
    nameToJob = Map(
      jobName -> WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          if isWindows then
            """@echo off
               |echo A=/%A%/
               |echo B=/%B%/
               |echo C=/%C%/
               |echo D=/%D%/
               |echo E=/%E%/
               |echo aJobResourceVariable=/%aJobResourceVariable%/
               |""".stripMargin
          else
            """#!/usr/bin/env bash
              |set -euo pipefail
              |echo A=/$A/
              |echo B=/$B/
              |echo C=/$C/
              |echo D=/$D/
              |echo E=/$E/
              |echo aJobResourceVariable=/$aJobResourceVariable/
              |""".stripMargin,
          env = Map(
            "D" -> expr("'D of JOB ENV'"),
            "E" -> expr("'E of JOB ENV'"),
            "aJobResourceVariable" -> expr("JobResource:JOB-RESOURCE-A:a"))),
        defaultArguments = Map("A" -> StringConstant("A of WorkflowJob")),
        jobResourcePaths = Seq(aJobResource.path, bJobResource.path))))

  private val envJobResource = JobResource(
    JobResourcePath("JOB-RESOURCE-ENV"),
    env = Map(
      "ORIGINAL_PATH" -> expr(s"env('$PathEnvName')")))

  private val envWorkflow = Workflow(
    WorkflowPath("WORKFLOW-ENV") ~ "INITIAL",
    Vector(Execute.Anonymous(
      WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          if isWindows then
            """@echo off
              |echo ORIGINAL_PATH=/%ORIGINAL_PATH%/
              |""".stripMargin
          else
            """#!/usr/bin/env bash
              |set -euo pipefail
              |echo ORIGINAL_PATH=/$ORIGINAL_PATH/
              |""".stripMargin),
        jobResourcePaths = Seq(envJobResource.path)))))

  private val sosJobResource = JobResource(
    JobResourcePath("JOB-RESOURCE-SOS"),
    env = Map(
      "JS7_ORDER_ID"          -> expr("$js7OrderId"),
      "JS7_WORKFLOW_NAME"     -> expr("$js7WorkflowPath"),
      "JS7_WORKFLOW_POSITION" -> expr("$js7WorkflowPosition"),
      "JS7_LABEL"             -> expr("$js7Label"),
      "JS7_JOB_NAME"          -> expr("$js7JobName"),
      "JS7_CONTROLLER_ID"     -> expr("$js7ControllerId"),
      "JS7_SCHEDULED_DATE"    -> expr("scheduledOrEmpty(format='yyyy-MM-dd HH:mm:ssZ')"),
      "JS7_SCHEDULED_YEAR"    -> expr("scheduledOrEmpty(format='yyyy')"),
      "JS7_SCHEDULED_MONTH"   -> expr("scheduledOrEmpty(format='MM')"),
      "JS7_SCHEDULED_DAY"     -> expr("scheduledOrEmpty(format='dd')"),
      "JS7_SCHEDULED_HOUR"    -> expr("scheduledOrEmpty(format='HH')"),
      "JS7_SCHEDULED_MINUTE"  -> expr("scheduledOrEmpty(format='mm')"),
      "JS7_SCHEDULED_SECOND"  -> expr("scheduledOrEmpty(format='ss')"),
      "JS7_JOBSTART_DATE"     -> expr("now(format='yyyy-MM-dd HH:mm:ssZ')"),
      "JS7_JOBSTART_DAY"      -> expr("now(format='dd')"),
      "JS7_JOBSTART_YEAR"     -> expr("now(format='yyyy')"),
      "JS7_JOBSTART_MONTH"    -> expr("now(format='MM')"),
      "JS7_JOBSTART_HOUR"     -> expr("now(format='HH')"),
      "JS7_JOBSTART_MINUTE"   -> expr("now(format='mm')"),
      "JS7_JOBSTART_SECOND"   -> expr("now(format='ss')")))
  logger.debug(sosJobResource.asJson.toPrettyString)

  private val sosWorkflow = {
    val jobName = WorkflowJob.Name("TEST-JOB")
    Workflow(
      WorkflowPath("WORKFLOW-SOS") ~ "INITIAL",
      Vector("TEST-LABEL" @: Execute.Named(jobName)),
      nameToJob = Map(jobName -> WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          if isWindows then
            """@echo off
              |echo JS7_ORDER_ID=/%JS7_ORDER_ID%/
              |echo JS7_WORKFLOW_NAME=/%JS7_WORKFLOW_NAME%/
              |echo JS7_WORKFLOW_POSITION=/%JS7_WORKFLOW_POSITION%/
              |echo JS7_LABEL=/%JS7_LABEL%/
              |echo JS7_JOB_NAME=/%JS7_JOB_NAME%/
              |echo JS7_CONTROLLER_ID=/%JS7_CONTROLLER_ID%/
              |echo JS7_SCHEDULED_DATE=/%JS7_SCHEDULED_DATE%/
              |echo JS7_SCHEDULED_DAY=/%JS7_SCHEDULED_DAY%/
              |echo JS7_SCHEDULED_MONTH=/%JS7_SCHEDULED_MONTH%/
              |echo JS7_SCHEDULED_YEAR=/%JS7_SCHEDULED_YEAR%/
              |echo JS7_SCHEDULED_HOUR=/%JS7_SCHEDULED_HOUR%/
              |echo JS7_SCHEDULED_MINUTE=/%JS7_SCHEDULED_MINUTE%/
              |echo JS7_SCHEDULED_SECOND=/%JS7_SCHEDULED_SECOND%/
              |echo JS7_JOBSTART_DATE=/%JS7_JOBSTART_DATE%/
              |echo JS7_JOBSTART_DAY=/%JS7_JOBSTART_DAY%/
              |echo JS7_JOBSTART_MONTH=/%JS7_JOBSTART_MONTH%/
              |echo JS7_JOBSTART_YEAR=/%JS7_JOBSTART_YEAR%/
              |echo JS7_JOBSTART_HOUR=/%JS7_JOBSTART_HOUR%/
              |echo JS7_JOBSTART_MINUTE=/%JS7_JOBSTART_MINUTE%/
              |echo JS7_JOBSTART_SECOND=/%JS7_JOBSTART_SECOND%/
              |""".stripMargin
          else
            """#!/usr/bin/env bash
              |set -euo pipefail
              |echo JS7_ORDER_ID=/$JS7_ORDER_ID/
              |echo JS7_WORKFLOW_NAME=/$JS7_WORKFLOW_NAME/
              |echo JS7_WORKFLOW_POSITION=/$JS7_WORKFLOW_POSITION/
              |echo JS7_LABEL=/$JS7_LABEL/
              |echo JS7_JOB_NAME=/$JS7_JOB_NAME/
              |echo JS7_CONTROLLER_ID=/$JS7_CONTROLLER_ID/
              |echo JS7_SCHEDULED_DATE=/$JS7_SCHEDULED_DATE/
              |echo JS7_SCHEDULED_DAY=/$JS7_SCHEDULED_DAY/
              |echo JS7_SCHEDULED_MONTH=/$JS7_SCHEDULED_MONTH/
              |echo JS7_SCHEDULED_YEAR=/$JS7_SCHEDULED_YEAR/
              |echo JS7_SCHEDULED_HOUR=/$JS7_SCHEDULED_HOUR/
              |echo JS7_SCHEDULED_MINUTE=/$JS7_SCHEDULED_MINUTE/
              |echo JS7_SCHEDULED_SECOND=/$JS7_SCHEDULED_SECOND/
              |echo JS7_JOBSTART_DATE=/$JS7_JOBSTART_DATE/
              |echo JS7_JOBSTART_DAY=/$JS7_JOBSTART_DAY/
              |echo JS7_JOBSTART_MONTH=/$JS7_JOBSTART_MONTH/
              |echo JS7_JOBSTART_YEAR=/$JS7_JOBSTART_YEAR/
              |echo JS7_JOBSTART_HOUR=/$JS7_JOBSTART_HOUR/
              |echo JS7_JOBSTART_MINUTE=/$JS7_JOBSTART_MINUTE/
              |echo JS7_JOBSTART_SECOND=/$JS7_JOBSTART_SECOND/
              |""".stripMargin))),
      jobResourcePaths = Seq(sosJobResource.path))
  }

  private val internalWorkflow = {
    val jobName = WorkflowJob.Name("TEST-JOB")
    Workflow(
      WorkflowPath("WORKFLOW-INTERNAL") ~ "INITIAL",
      Vector("TEST-LABEL" @: Execute.Named(jobName)),
      nameToJob = Map(
        jobName -> TestJob.workflowJob(agentPath)),
      jobResourcePaths = Seq(aJobResource.path, bJobResource.path))
  }

  private class TestJob extends InternalJob {
    def toOrderProcess(step: Step) =
      OrderProcess(IO {
        assert(step.jobResourceVariable(aJobResource.path, "a") == Right(StringValue("A of JOB-RESOURCE-A")))
        assert(step.jobResourceVariable(aJobResource.path, "UNKNOWN") == Left(UnknownKeyProblem("JobResource variable", "UNKNOWN")))
        assert(step.jobResourceVariable(JobResourcePath("UNKNOWN"), "X") == Left(UnknownKeyProblem("JobResource", "UNKNOWN")))
        Outcome.succeeded
      })
  }
  private object TestJob extends InternalJob.Companion[TestJob]
}
