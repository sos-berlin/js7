package js7.tests.filewatch

import java.nio.file.Files.{createDirectory, exists}
import js7.agent.scheduler.order.FileWatchManager
import js7.base.configutils.Configs._
import js7.base.generic.Completed
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.agent.AgentId
import js7.data.item.CommonItemEvent.{ItemAttached, ItemDestroyed}
import js7.data.item.ItemOperation.SimpleDelete
import js7.data.order.OrderEvent.OrderRemoved
import js7.data.order.OrderId
import js7.data.orderwatch.{FileWatch, OrderWatchId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatchLongTest._
import js7.tests.jobs.DeleteFileJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class FileWatchLongTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = Seq(agentId)
  protected val versionedItems = Seq(workflow)

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.filewatch.poll-timeout = 0ms
    js7.filewatch.retry-delays = [0ms]
    """

  private val sourceDirectory = directoryProvider.agents(0).dataDir / "tmp/files"

  private lazy val fileWatch = FileWatch(
    OrderWatchId("TEST-WATCH"),
    workflow.path,
    agentId,
    sourceDirectory.toString)

  private def fileToOrderId(filename: String): OrderId =
    FileWatchManager.relativePathToOrderId(fileWatch, filename).get.orThrow

  "Start with a filel" in {
    createDirectory(sourceDirectory)
    controllerApi.updateUnsignedSimpleItems(Seq(fileWatch)).await(99.s).orThrow
    val file = sourceDirectory / "1"
    val orderId = fileToOrderId("1")
    file := ""
    controller.eventWatch.await[ItemAttached](_.event.id == fileWatch.id)
    controller.eventWatch.await[OrderRemoved](_.key == orderId)
    assert(!exists(file))
  }

  "Wait some time with tiny poll interval" in {
    // Due to tiny poll interval the polling loop (or recursion) has a lot of iterations
    sleep(5.s)
  }

  "Delete FileWatch" in {
    assert(controllerApi.updateItems(Observable(SimpleDelete(fileWatch.id))).await(99.s) ==
      Right(Completed))
    controller.eventWatch.await[ItemDestroyed](_.event.id == fileWatch.id)
    assert(controller.controllerState.await(99.s).allOrderWatchesState.idToOrderWatchState.isEmpty)
  }
}

object FileWatchLongTest
{
  private val agentId = AgentId("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(DeleteFileJob.execute(agentId)))
}
