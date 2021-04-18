package js7.tests.filewatch

import java.nio.file.Files.createDirectory
import js7.agent.scheduler.order.FileWatchManager
import js7.base.configutils.Configs._
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.SimplePattern
import js7.data.agent.AgentId
import js7.data.event.EventId
import js7.data.item.CommonItemEvent.ItemAttached
import js7.data.job.InternalExecutable
import js7.data.order.OrderEvent.{OrderRemoved, OrderStarted}
import js7.data.order.{OrderId, Outcome}
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.tests.filewatch.FileWatchNarrowPatternTest._
import js7.tests.jobs.DeleteFileJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.catnap.Semaphore
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class FileWatchNarrowPatternTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = Seq(agentId)
  protected val versionedItems = Seq(workflow)
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  private val sourceDirectory = directoryProvider.agents(0).dataDir / "tmp/files"

  private lazy val fileWatch = FileWatch(
    OrderWatchId("TEST-WATCH"),
    workflow.path,
    agentId,
    sourceDirectory.toString)

  private def fileToOrderId(filename: String): OrderId =
    FileWatchManager.relativePathToOrderId(fileWatch, filename).get.orThrow

  private val aFile = sourceDirectory / "A"
  private val aOrderId = fileToOrderId("A")
  private val bFile = sourceDirectory / "NARROW-B"
  private val bOrderId = fileToOrderId("NARROW-B")

  "Add two files" in {
    createDirectory(sourceDirectory)
    controllerApi.updateUnsignedSimpleItems(Seq(fileWatch)).await(99.s).orThrow
    controller.eventWatch.await[ItemAttached](_.event.id == fileWatch.id)

    // Add one by one to circument AgentOrderKeeper's problem with multiple orders (JobActorStarvationTest)
    aFile := ""
    controller.eventWatch.await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("A"))
    controller.eventWatch.await[OrderStarted](_.key == aOrderId)

    bFile := ""
    controller.eventWatch.await[ExternalOrderArised](_.event.externalOrderName == ExternalOrderName("NARROW-B"))
    controller.eventWatch.await[OrderStarted](_.key == bOrderId)
  }

  "Narrow the pattern" in {
    val eventId = controller.eventWatch.lastAddedEventId
    val changedFileWatch = fileWatch.copy(pattern = Some(SimplePattern("NARROW-.+")))
    controllerApi.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
    controller.eventWatch.await[ItemAttached](_.event.id == fileWatch.id, after = eventId)

    // Now, the A file is not match and out of scope, and a ExternalOrderVanished is emitted.

    controller.eventWatch.await[ExternalOrderVanished](_.event.externalOrderName == ExternalOrderName("A"))
    // This must be the only ExternalOrderVanished event
    sleep(100.ms)
    assert(controller.eventWatch.keyedEvents[ExternalOrderVanished](after = EventId.BeforeFirst) ==
      Seq(fileWatch.id <-: ExternalOrderVanished(ExternalOrderName("A"))))

    semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
    controller.eventWatch.await[OrderRemoved](_.key == aOrderId)
    controller.eventWatch.await[OrderRemoved](_.key == bOrderId)
  }
}

object FileWatchNarrowPatternTest
{
  private val agentId = AgentId("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(agentId, InternalExecutable(classOf[SemaphoreJob].getName), taskLimit = 10)),
      Execute(WorkflowJob(agentId, InternalExecutable(classOf[DeleteFileJob].getName), taskLimit = 10))))

  private val semaphore = Semaphore[Task](0).memoize

  final class SemaphoreJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess(
        semaphore.flatMap(_.acquire)
          .as(Outcome.succeeded))
  }
}
