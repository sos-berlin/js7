package js7.tests

import js7.base.configutils.Configs._
import js7.base.problem.Checked._
import js7.base.system.OperatingSystem.isMac
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.data.agent.AgentId
import js7.data.job.InternalExecutable
import js7.data.order.OrderEvent.{OrderFinished, OrderStarted}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.tests.JobActorStarvationTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.catnap.Semaphore
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.TimeoutException

final class JobActorStarvationTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = Seq(agentId)
  protected val versionedItems = Seq(workflow)
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  "Add a order to start AgentDriver CommandQueue" in {
    val zeroOrderId = OrderId("0")
    controller.addOrderBlocking(FreshOrder(zeroOrderId, workflow.path))
    semaphore.flatMap(_.releaseN(1)).runSyncUnsafe()
    controller.eventWatch.await[OrderFinished](_.key == zeroOrderId)
  }

  "Add two orders simultaneously and hold them in the job" in {
    if (!isMac) pending // Test does not work under Liunx
    pending // check again !!!

    val aOrderId = OrderId("A")
    val bOrderId = OrderId("B")
    // FIXME Problem in protocol between AgentOrderKeeper and JobActor
    // Possible solution: replace Actors by Tasks!
    // AgentDriver transfers the two orders in one AgentCommand.Batch
    // AgentOrderKeeper processes these added orders at once
    // but sends only one to the JobActor,
    // letting the other starve until the JobActor requests the next order.
    controllerApi
      .addOrders(Observable(
        FreshOrder(aOrderId, workflow.path),
        FreshOrder(bOrderId, workflow.path)))
      .await(99.s).orThrow
    val startedOrderId = controller.eventWatch.await[OrderStarted](ke => ke.key == aOrderId || ke.key == bOrderId)
      .head.value.key
    val otherOrderId = (startedOrderId: @unchecked) match {
      case `aOrderId` => bOrderId
      case `bOrderId` => aOrderId
    }
    intercept[TimeoutException] {
      controller.eventWatch.await[OrderStarted](_.key == otherOrderId, timeout = 1.s)
    }
    System.err.println("JobActorStarvationTest: Second order is starving in AgentOrderKeeper")

    semaphore.flatMap(_.releaseN(2)).runSyncUnsafe()
    controller.eventWatch.await[OrderFinished](_.key == aOrderId)
    controller.eventWatch.await[OrderFinished](_.key == bOrderId)
  }
}

object JobActorStarvationTest
{
  private val agentId = AgentId("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(agentId, InternalExecutable(classOf[SemaphoreJob].getName), taskLimit = 2))))

  private val semaphore = Semaphore[Task](0).memoize

  final class SemaphoreJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess(
        semaphore.flatMap(_.acquire)
          .as(Outcome.succeeded))
  }
}
