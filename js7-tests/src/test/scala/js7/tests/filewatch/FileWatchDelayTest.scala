package js7.tests.filewatch

import cats.instances.vector._
import cats.syntax.parallel._
import java.nio.file.Files.{createDirectories, exists}
import js7.agent.scheduler.order.FileWatchManager
import js7.base.configutils.Configs._
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked._
import js7.base.system.OperatingSystem.isMac
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.data.agent.AgentId
import js7.data.event.EventRequest
import js7.data.item.SimpleItemEvent.SimpleItemAttached
import js7.data.order.OrderEvent.{OrderFinished, OrderRemoved}
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchEvent.ExternalOrderArised
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatchDelayTest._
import js7.tests.jobs.DeleteFileJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now

final class FileWatchDelayTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""

  override protected def agentConfig = config"""
    js7.filewatch.watch-delay = 0ms
    js7.journal.remove-obsolete-files = false
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentIds = Seq(agentId)
  protected val versionedItems = Seq(workflow)

  private lazy val watchedDirectory = directoryProvider.agents(0).dataDir / "tmp/a-files"
  private val orderWatchId = OrderWatchId("FILE-WATCH")

  private lazy val fileWatch = FileWatch(
    orderWatchId,
    workflow.path,
    agentId,
    watchedDirectory.toString,
    delay = systemWatchDelay + 300.ms)

  private def fileToOrderId(filename: String): OrderId =
    FileWatchManager.relativePathToOrderId(fileWatch, filename).get.orThrow

  import controller.eventWatch
  import controller.eventWatch.await

  "Start with some files" in {
    createDirectories(watchedDirectory)
    controllerApi.updateSimpleItems(Seq(fileWatch)).await(99.s).orThrow
    await[SimpleItemAttached](_.event.id == orderWatchId)

    // Each test has an increasing sequence of file modifications, delaying FileAdded and OrderAdded.
    def delayedFileAddedTest(i: Int) = Task {
      withClue(s"#$i") {
        val file = watchedDirectory / i.toString
        val externalOrderName = ExternalOrderName(i.toString)
        val orderId = fileToOrderId(i.toString)
        val whenArised = eventWatch
          .whenKeyedEvent[ExternalOrderArised](
            EventRequest.singleClass(after = eventWatch.lastAddedEventId, timeout = Some(99.s)),
            key = orderWatchId,
            predicate = _.externalOrderName == externalOrderName)
          .runToFuture
        val since = now

        file := ""
        sleep(systemWatchDelay + 100.ms)
        file ++= "A"
        assert(!whenArised.isCompleted)
        for (_ <- 0 to 2 * i) {
          sleep(fileWatch.delay / 2)
          file ++= "+"
          assert(!whenArised.isCompleted)
        }
        whenArised.await(99.s)
        assert(since.elapsed >= systemWatchDelay + i * fileWatch.delay)
        await[OrderFinished](_.key == orderId)
        await[OrderRemoved](_.key == orderId)
        assert(!exists(file))
      }
    }

    // Run tests in parallel
    (0 to 4).toVector
      .parUnorderedTraverse(delayedFileAddedTest)
      .await(99.s)
  }
}

object FileWatchDelayTest
{
  private val agentId = AgentId("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      DeleteFileJob.execute(agentId)))

  private val systemWatchDelay = if (isMac) 2.s else 0.s
}
