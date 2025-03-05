package js7.tests.filewatch

import java.nio.file.Files.createDirectory
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.SimplePattern
import js7.data.agent.AgentPath
import js7.data.event.EventId
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.order.OrderEvent.{OrderDeleted, OrderStarted}
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchPath}
import js7.data.plan.PlanId
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatchNarrowPatternTest.*
import js7.tests.jobs.{DeleteFileJob, SemaphoreJob}
import js7.tests.testenv.ControllerAgentForScalaTest

final class FileWatchNarrowPatternTest extends OurTestSuite, ControllerAgentForScalaTest:

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  private val sourceDirectory = directoryProvider.agentEnvs(0).dataDir / "work/files"

  private lazy val fileWatch = FileWatch(
    OrderWatchPath("TEST-WATCH"),
    workflow.path,
    agentPath,
    StringConstant(sourceDirectory.toString))

  private def externalToOrderId(externalOrderName: ExternalOrderName): OrderId =
    val (orderId, planId) =
      fileWatch.externalToOrderAndPlanId(externalOrderName, None, Timestamp.now).orThrow
    assert(planId == PlanId.Global)
    orderId

  private val aFile = sourceDirectory / "A"
  private val aOrderId = externalToOrderId(ExternalOrderName("A"))
  private val bFile = sourceDirectory / "NARROW-B"
  private val bOrderId = externalToOrderId(ExternalOrderName("NARROW-B"))

  "Add two files" in:
    createDirectory(sourceDirectory)
    controller.api.updateUnsignedSimpleItems(Seq(fileWatch)).await(99.s).orThrow
    eventWatch.await[ItemAttached](_.event.key == fileWatch.path)

    // Add one by one to circument AgentOrderKeeper's problem with multiple orders (JobDriverStarvationTest)
    aFile := ""
    eventWatch.await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("A"))
    eventWatch.await[OrderStarted](_.key == aOrderId)

    bFile := ""
    eventWatch.await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("NARROW-B"))
    eventWatch.await[OrderStarted](_.key == bOrderId)

  "Narrow the pattern" in:
    val eventId = eventWatch.lastAddedEventId
    val changedFileWatch = fileWatch.copy(pattern = Some(SimplePattern("NARROW-.+")))
    controller.api.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
    eventWatch.await[ItemAttached](_.event.key == fileWatch.path, after = eventId)

    // Now, the A file is not matched and out of scope, and a ExternalOrderVanished is emitted.

    eventWatch.await[ExternalOrderVanished](_.event.externalOrderName == ExternalOrderName("A"))
    // This must be the only ExternalOrderVanished event
    assert(eventWatch.keyedEvents[ExternalOrderVanished](after = EventId.BeforeFirst) ==
      Seq(fileWatch.path <-: ExternalOrderVanished(ExternalOrderName("A"))))

    TestJob.continue(2)
    eventWatch.await[OrderDeleted](_.key == aOrderId)
    eventWatch.await[OrderDeleted](_.key == bOrderId)


object FileWatchNarrowPatternTest:
  private val agentPath = AgentPath("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      TestJob.execute(agentPath, processLimit = 10),
      DeleteFileJob.execute(agentPath, processLimit = 10)))

  private class TestJob extends SemaphoreJob(TestJob)
  private object TestJob extends SemaphoreJob.Companion[TestJob]
