package js7.tests

import js7.base.configutils.Configs.*
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.{OrderDeleted, OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.JobDriverStarvationTest.*
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.ControllerAgentForScalaTest
import fs2.{Chunk, Stream}
import js7.base.monixlike.MonixLikeExtensions.completedL
import scala.concurrent.duration.Deadline.now

final class JobDriverStarvationTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected def controllerConfig = config"""
    js7.journal.slow-check-state = off
    """
  override protected def agentConfig = config"""
    js7.journal.slow-check-state = off
    js7.job.execution.signed-script-injection-allowed = on
    """
  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)

  "Add an order to start AgentDriver CommandQueue" in:
    val zeroOrderId = OrderId("0")
    controller.addOrderBlocking(FreshOrder(zeroOrderId, workflow.path, deleteWhenTerminated = true))
    TestJob.continue()
    eventWatch.await[OrderFinished](_.key == zeroOrderId)
    eventWatch.await[OrderDeleted](_.key == zeroOrderId)

  "Add two orders simultaneously and hold them in the job" in:
    // TODO ? Problem in protocol between AgentOrderKeeper and JobActor
    // Solution: replace Actors by Tasks!
    // AgentDriver transfers the two orders in one AgentCommand.Batch
    // AgentOrderKeeper processes these added orders at once
    // but sends only one to the JobActor,
    // letting the other starve until the JobActor requests the next order.

    val orderIds = for i <- 1 to n yield OrderId(s"ORDER-$i")
    val proxy = controller.api.startProxy().await(99.s)

    val firstOrdersProcessing = proxy.stream()
      .map(_.stampedEvent.value)
      .collect:
        case KeyedEvent(orderId: OrderId, _: OrderProcessingStarted) => orderId
      .take(processLimit)
      .completedL
      .unsafeToFuture()

    val allOrdersDeleted = proxy.stream()
      .map(_.stampedEvent.value)
      .collect:
        case KeyedEvent(orderId: OrderId, _: OrderDeleted) => orderId
      .scan(orderIds.toSet)(_ - _).drop(1)
      .takeWhile(_.nonEmpty)
      .completedL
      .unsafeToFuture()

    var t = now
    controller.api
      .addOrders(Stream
        .chunk(Chunk.from(orderIds))
        .mapChunks(_.map:
          FreshOrder(_, workflow.path, deleteWhenTerminated = true)))
      .await(99.s).orThrow
    firstOrdersProcessing.await(99.s)
    logger.info("🔷 " + itemsPerSecondString(t.elapsed, n, "started"))

    t = now
    TestJob.continue(orderIds.size)
    allOrdersDeleted.await(99.s)
    logger.info("🔷 " + itemsPerSecondString(t.elapsed, n, "completed"))

    proxy.stop.await(99.s)


object JobDriverStarvationTest:
  private val logger = Logger[this.type]
  private val n = 10_000
  private val processLimit = 97 min n
  private val agentPath = AgentPath("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      TestJob.execute(agentPath, processLimit = processLimit)))

  private class TestJob extends SemaphoreJob(TestJob)
  private object TestJob extends SemaphoreJob.Companion[TestJob]
