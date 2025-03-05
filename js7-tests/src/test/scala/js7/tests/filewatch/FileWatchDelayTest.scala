package js7.tests.filewatch

import cats.effect.IO
import cats.instances.vector.*
import cats.syntax.parallel.*
import java.nio.file.Files.{createDirectories, exists}
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.watch.BasicDirectoryWatch.systemWatchDelay
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.data.agent.AgentPath
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.order.OrderEvent.{OrderDeleted, OrderFinished}
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchEvent.ExternalOrderArised
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchPath}
import js7.data.plan.PlanId
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatchDelayTest.*
import js7.tests.jobs.DeleteFileJob
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now

final class FileWatchDelayTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.directory-watch.watch-delay = 0ms
    js7.directory-watch.log-delays = [ 0s, 1s, 2s ]
    js7.journal.remove-obsolete-files = false
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)

  private lazy val watchedDirectory = directoryProvider.agentEnvs(0).dataDir / "tmp/a-files"
  private val orderWatchPath = OrderWatchPath("FILE-WATCH")

  private val writeDuration = 2 * systemWatchDelay + 500.ms

  private lazy val fileWatch = FileWatch(
    orderWatchPath,
    workflow.path,
    agentPath,
    StringConstant(watchedDirectory.toString),
    delay = writeDuration + 1.s)

  private def externalToOrderId(externalOrderName: ExternalOrderName): OrderId =
    val (orderId, planId) =
      fileWatch.externalToOrderAndPlanId(externalOrderName, None, Timestamp.now).orThrow
    assert(planId == PlanId.Global)
    orderId

  import eventWatch.await

  "Start with some files" in:
    createDirectories(watchedDirectory)
    controller.api.updateUnsignedSimpleItems(Seq(fileWatch)).await(99.s).orThrow
    await[ItemAttached](_.event.key == orderWatchPath)

    // Each test has an increasing sequence of file modifications, delaying FileAdded and OrderAdded.
    def delayedFileAddedTest(i: Int) = IO:
      val filename = s"file-$i:"
      withClue(filename):
        val file = watchedDirectory / filename
        val externalOrderName = ExternalOrderName(filename)
        val orderId = externalToOrderId(externalOrderName)
        val whenArised = eventWatch
          .awaitAsync[ExternalOrderArised](
            stamped =>
              stamped.key == orderWatchPath && stamped.event.externalOrderName == externalOrderName,
            after = eventWatch.lastAddedEventId)
          .unsafeToFuture()

        logger.info(s"""file-$i := """"")
        val since = now
        file := ""
        sleepUntil(since + systemWatchDelay + 100.ms)

        logger.info(s"""file-$i ++= "A" +${since.elapsed.pretty}""")
        file ++= "A"
        assert(!whenArised.isCompleted)
        val divisor = 4
        for j <- 1 to i * divisor do withClue(s"#$i"):
          sleepUntil(since + systemWatchDelay + j * writeDuration / divisor)
          logger.info(s"""file-$i ++= "${"+" * j}" +${since.elapsed.pretty}""")
          file ++= "+"
          assert(!whenArised.isCompleted)
        whenArised.await(99.s)
        val expectedDuration = fileWatch.delay + i * writeDuration
        val duration = since.elapsed
        // TODO On macOS, files arise 3s or 4s later then expected
        logger.info(s"file-$i arised ⭐️ after ${duration.pretty}, " +
          s"${(duration - expectedDuration).pretty} later than expected")
        assert(duration >= expectedDuration)
        await[OrderFinished](_.key == orderId)
        await[OrderDeleted](_.key == orderId)
        assert(!exists(file))

    // Run tests in parallel
    (0 to 4).toVector
      .parUnorderedTraverse(delayedFileAddedTest)
      .await(99.s)


object FileWatchDelayTest:
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      DeleteFileJob.execute(agentPath)))
