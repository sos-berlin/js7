package js7.tests.filewatch

import cats.instances.vector.*
import cats.syntax.parallel.*
import java.nio.file.Files.{createDirectories, exists}
import js7.agent.scheduler.order.FileWatchManager
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.watch.BasicDirectoryWatcher.systemWatchDelay
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.event.EventRequest
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.order.OrderEvent.{OrderDeleted, OrderFinished}
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchEvent.ExternalOrderArised
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchPath}
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatchDelayTest.*
import js7.tests.jobs.DeleteFileJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.duration.Deadline.now

final class FileWatchDelayTest extends OurTestSuite with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.filewatch.watch-delay = 0ms
    js7.filewatch.log-delays = [ 0s, 1s, 2s ]
    js7.journal.remove-obsolete-files = false
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)

  private lazy val watchedDirectory = directoryProvider.agents(0).dataDir / "tmp/a-files"
  private val orderWatchPath = OrderWatchPath("FILE-WATCH")

  private val writeDuration = systemWatchDelay + 1.s

  private lazy val fileWatch = FileWatch(
    orderWatchPath,
    workflow.path,
    agentPath,
    StringConstant(watchedDirectory.toString),
    delay = writeDuration)

  private def fileToOrderId(filename: String): OrderId =
    FileWatchManager.relativePathToOrderId(fileWatch, filename).get.orThrow

  import eventWatch.await

  "Start with some files" in {
    createDirectories(watchedDirectory)
    controllerApi.updateUnsignedSimpleItems(Seq(fileWatch)).await(99.s).orThrow
    await[ItemAttached](_.event.key == orderWatchPath)

    // Each test has an increasing sequence of file modifications, delaying FileAdded and OrderAdded.
    def delayedFileAddedTest(i: Int) = Task {
      val filename = s"file-$i"
      withClue(filename) {
        val file = watchedDirectory / filename
        val externalOrderName = ExternalOrderName(filename)
        val orderId = fileToOrderId(filename)
        val whenArised = eventWatch
          .whenKeyedEvent[ExternalOrderArised](
            EventRequest.singleClass(after = eventWatch.lastAddedEventId, timeout = Some(99.s)),
            key = orderWatchPath,
            predicate = _.externalOrderName == externalOrderName)
          .runToFuture
        val since = now

        logger.info(s"""file-$i := """"")
        file := ""
        sleepUntil(since + systemWatchDelay + 100.ms)

        logger.info(s"""file-$i ++= "A"""")
        file ++= "A"
        assert(!whenArised.isCompleted)
        val divisor = 4
        for (j <- 1 to i * divisor) withClue(s"#$i") {
          sleepUntil(since + j * writeDuration / divisor)
          logger.info(s"""file-$i ++= "${"+" * j}"""")
          file ++= "+"
          assert(!whenArised.isCompleted)
        }
        whenArised.await(99.s)
        assert(since.elapsed >= systemWatchDelay + i * writeDuration)
        await[OrderFinished](_.key == orderId)
        await[OrderDeleted](_.key == orderId)
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
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      DeleteFileJob.execute(agentPath)))
}
