package js7.tests

import js7.base.configutils.Configs._
import js7.base.log.Logger
import js7.base.problem.Checked._
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.job.InternalExecutable
import js7.data.order.OrderEvent.{OrderDeleted, OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.tests.JobDriverStarvationTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.catnap.Semaphore
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now

final class JobDriverStarvationTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected def controllerConfig = config"""
    js7.journal.slow-check-state = off
    """
  override protected def agentConfig = config"""
    js7.journal.slow-check-state = off
    js7.job.execution.signed-script-injection-allowed = on
    """
  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)

  "Add a order to start AgentDriver CommandQueue" in {
    val zeroOrderId = OrderId("0")
    controller.addOrderBlocking(FreshOrder(zeroOrderId, workflow.path, deleteWhenTerminated = true))
    semaphore.flatMap(_.releaseN(1)).runSyncUnsafe()
    eventWatch.await[OrderFinished](_.key == zeroOrderId)
    eventWatch.await[OrderDeleted](_.key == zeroOrderId)
  }

  "Add two orders simultaneously and hold them in the job" in {
    // TODO ? Problem in protocol between AgentOrderKeeper and JobActor
    // Solution: replace Actors by Tasks!
    // AgentDriver transfers the two orders in one AgentCommand.Batch
    // AgentOrderKeeper processes these added orders at once
    // but sends only one to the JobActor,
    // letting the other starve until the JobActor requests the next order.

    val orderIds = for (i <- 1 to n) yield OrderId(s"ORDER-$i")
    val proxy = controllerApi.startProxy().await(99.s)
    val firstOrdersProcessing = proxy.observable
      .map(_.stampedEvent.value)
      .collect {
        case KeyedEvent(orderId: OrderId, _: OrderProcessingStarted) => orderId
      }
      .take(parallelism)
      .completedL
      .runToFuture
    val allOrdersDeleted = proxy.observable
      .map(_.stampedEvent.value)
      .collect {
        case KeyedEvent(orderId: OrderId, _: OrderDeleted) => orderId
      }
      .scan0(orderIds.toSet)(_ - _)
      .takeWhile(_.nonEmpty)
      .completedL
      .runToFuture

    var t = now
    controllerApi
      .addOrders(Observable
        .fromIterable(orderIds)
        .map(FreshOrder(_, workflow.path, deleteWhenTerminated = true)))
      .await(99.s).orThrow
    // Seems to work now: intercept[TimeoutException] {
      firstOrdersProcessing.await(99.s)
    //}
    //System.err.println("JobDriverStarvationTest: Second order is starving in AgentOrderKeeper")
    logger.info("🔵 " + itemsPerSecondString(t.elapsed, n, "started"))

    t = now
    semaphore.flatMap(_.releaseN(orderIds.size)).runSyncUnsafe()
    allOrdersDeleted.await(99.s)
    logger.info("🔵 " + itemsPerSecondString(t.elapsed, n, "completed"))

    proxy.stop.await(99.s)
  }
}

object JobDriverStarvationTest
{
  private val logger = Logger[this.type]
  private val n = 10_000
  private val parallelism = 97
  private val agentPath = AgentPath("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      Execute(
        WorkflowJob(
          agentPath,
          InternalExecutable(classOf[SemaphoreJob].getName),
          parallelism = parallelism))))

  private val semaphore = Semaphore[Task](0).memoize

  final class SemaphoreJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess(
        semaphore.flatMap(_.acquire)
          .as(Outcome.succeeded))
  }
}