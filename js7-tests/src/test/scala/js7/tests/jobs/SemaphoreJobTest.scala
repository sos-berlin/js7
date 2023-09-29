package js7.tests.jobs

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.{OrderProcessed, OrderProcessingStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJobTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.TimeoutException

final class SemaphoreJobTest extends OurTestSuite with ControllerAgentForScalaTest:
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = true
    """
  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow)

  "continue and reset" in:
    assert(TestSemaphoreJob.semaphore.flatMap(_.count).await(99.s) == 0)
    val orderId = OrderId("ORDER")
    controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow

    controller.eventWatch.await[OrderProcessingStarted](_.key == orderId)
    controller.eventWatch.await[OrderStdoutWritten](_.key == orderId)
    intercept[TimeoutException](
      controller.eventWatch.await[OrderProcessed](_.key == orderId, timeout = 1.s))
    assert(TestSemaphoreJob.semaphore.flatMap(_.count).await(99.s) == -1)

    TestSemaphoreJob.continue(1)
    controller.eventWatch.await[OrderProcessed](_.key == orderId)

    assert(TestSemaphoreJob.semaphore.flatMap(_.count).await(99.s) == 0)

    TestSemaphoreJob.continue(3)
    assert(TestSemaphoreJob.semaphore.flatMap(_.count).await(99.s) == +3)

    TestSemaphoreJob.reset()
    assert(TestSemaphoreJob.semaphore.flatMap(_.count).await(99.s) == 0)

  "reset while job is waiting" in:
    val orderId = OrderId("ORDER-2")
    controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow

    controller.eventWatch.await[OrderProcessingStarted](_.key == orderId)
    controller.eventWatch.await[OrderStdoutWritten](_.key == orderId)
    intercept[TimeoutException](
      controller.eventWatch.await[OrderProcessed](_.key == orderId, timeout = 1.s))
    assert(TestSemaphoreJob.semaphore.flatMap(_.count).await(99.s) == -1)

    TestSemaphoreJob.reset()
    controller.eventWatch.await[OrderProcessed](_.key == orderId)

    assert(TestSemaphoreJob.semaphore.flatMap(_.count).await(99.s) == 0)

object SemaphoreJobTest:
  private val agentPath = AgentPath("AGENT")
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
