package js7.tests.filewatch

import fs2.Stream
import java.nio.file.Files.{createDirectory, exists}
import js7.base.configutils.Configs.*
import js7.base.generic.Completed
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.data.agent.AgentPath
import js7.data.item.BasicItemEvent.{ItemAttached, ItemDeleted}
import js7.data.item.ItemOperation.DeleteSimple
import js7.data.order.OrderEvent.OrderDeleted
import js7.data.order.OrderId
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchPath, OrderWatchState}
import js7.data.plan.PlanId
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatchLongTest.*
import js7.tests.jobs.DeleteFileJob
import js7.tests.testenv.ControllerAgentForScalaTest

final class FileWatchLongTest extends OurTestSuite, ControllerAgentForScalaTest:

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.directory-watch.poll-timeout = 10ms  # >0 too let Controller breath
    js7.directory-watch.retry-delays = [0ms]
    """

  private val sourceDirectory = directoryProvider.agentEnvs(0).dataDir / "work/files"

  private lazy val fileWatch = FileWatch(
    OrderWatchPath("TEST-WATCH"),
    workflow.path,
    agentPath,
    StringConstant(sourceDirectory.toString))

  private def externalToOrderId(externalOrderName: ExternalOrderName): OrderId =
    val (orderId, planId, priority) =
      fileWatch.externalToOrderAndPlanIdAndPriority(externalOrderName, None, Timestamp.now).orThrow
    assert(planId == PlanId.Global && priority == 0)
    orderId

  "Start with a file" in:
    createDirectory(sourceDirectory)
    controller.api.updateUnsignedSimpleItems(Seq(fileWatch)).await(99.s).orThrow
    val file = sourceDirectory / "1"
    val orderId = externalToOrderId(ExternalOrderName("1"))
    file := ""
    eventWatch.await[ItemAttached](_.event.key == fileWatch.path)
    eventWatch.await[OrderDeleted](_.key == orderId)
    assert(!exists(file))

  "Wait some time with tiny poll interval" in:
    // Due to tiny poll interval the polling loop (or recursion) has a lot of iterations
    sleep(5.s)

  "Delete FileWatch" in:
    assert(controller.api.updateItems(Stream(DeleteSimple(fileWatch.path))).await(99.s) ==
      Right(Completed))
    eventWatch.await[ItemDeleted](_.event.key == fileWatch.path)
    assert(controllerState.keyTo(OrderWatchState).isEmpty)


object FileWatchLongTest:
  private val agentPath = AgentPath("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(DeleteFileJob.execute(agentPath)))
