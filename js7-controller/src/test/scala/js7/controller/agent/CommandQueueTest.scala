package js7.controller.agent

import com.typesafe.scalalogging.{Logger => ScalaLogger}
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.Batch
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.time.WaitForCondition.waitForCondition
import js7.controller.agent.AgentDriver.{Input, Queueable}
import js7.controller.agent.CommandQueue.QueuedInputResponse
import js7.controller.agent.CommandQueueTest._
import js7.data.agent.AgentPath
import js7.data.job.PathExecutable
import js7.data.order.{Order, OrderId, OrderMark}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.AtomicInt
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable
import scala.language.reflectiveCalls

/**
  * @author Joacim Zschimmer
  */
final class CommandQueueTest extends AnyFreeSpec
{
  "test" in {
    val commandQueue = new MyCommandQueue(logger, batchSize = 3) {
      val succeeded = mutable.Buffer[Seq[QueuedInputResponse]]()
      val failed = mutable.Buffer[(Vector[Queueable], Problem)]()

      protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
        succeeded += queuedInputResponses

      protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem) =
        failed += ((inputs, problem))
    }

    val expected = mutable.Buffer[Seq[QueuedInputResponse]]()

    commandQueue.onCoupled(Set.empty)

    // The first Input is sent alone to the Agent regardless of commandBatchSize.
    val aOrder = toOrder("A")
    var ok = commandQueue.enqueue(AgentDriver.Input.AttachOrder(aOrder, TestAgentId))
    assert(ok)

    // Duplicate
    ok = commandQueue.enqueue(AgentDriver.Input.AttachOrder(aOrder, TestAgentId))
    assert(!ok)

    expected += toQueuedInputResponse(aOrder) :: Nil
    waitForCondition(99.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    val twoOrders = toOrder("B") :: toOrder("C") :: Nil
    for (o <- twoOrders) commandQueue.enqueue(AgentDriver.Input.AttachOrder(o, TestAgentId))
    waitForCondition(99.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    // After the Agent has processed the Input, the two queued commands are sent as a Batch to the Agent
    commandQueue.handleBatchSucceeded(commandQueue.succeeded.last) shouldEqual List(Input.AttachOrder(aOrder, TestAgentId))
    expected += twoOrders map toQueuedInputResponse
    waitForCondition(99.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    val fiveOrders = toOrder("D") :: toOrder("E") :: toOrder("F") :: toOrder("G") :: toOrder("H") :: Nil
    for (o <- fiveOrders) commandQueue.enqueue(AgentDriver.Input.AttachOrder(o, TestAgentId))
    expected += fiveOrders take 1 map toQueuedInputResponse
    waitForCondition(99.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    // After the Agent has processed the Input, three of the queued commands are sent as a Batch to the Agent
    commandQueue.handleBatchSucceeded(commandQueue.succeeded.last) shouldEqual fiveOrders.take(1).map(o => Input.AttachOrder(o, TestAgentId))
    expected += fiveOrders drop 1 take 3 map toQueuedInputResponse
    waitForCondition(99.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    // Finally, the last queued Input is processed
    commandQueue.handleBatchSucceeded(commandQueue.succeeded.last)
    expected += fiveOrders drop 4 map toQueuedInputResponse
    waitForCondition(99.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)
    assert(commandQueue.failed.isEmpty)
  }

  "Duplicate MarkOrder" in {
    // ControllerOrderQueue may send MarkOrder for each OrderEvent received from Agent
    // because MarkOrder is executeted asynchronously and effect occurs later.
    val queue = new MyCommandQueue(logger, batchSize = 3) {
      protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) = ()
      protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem) = ()
    }
    var ok = queue.enqueue(Input.MarkOrder(OrderId("ORDER"), OrderMark.Suspending()))
    assert(ok)
    ok = queue.enqueue(Input.MarkOrder(OrderId("ORDER"), OrderMark.Suspending()))
    assert(!ok)
    ok = queue.enqueue(Input.MarkOrder(OrderId("ORDER"), OrderMark.Resuming()))
    assert(ok)
  }

  "Performance with any orders" in {
    // Run with -DCommandQueueTest=10000000 (10 million) -Xmx3g
    val n = sys.props.get("CommandQueueTest").map(_.toInt) getOrElse 10000
    val orders = for (i <- 1 to n) yield toOrder(i.toString)
    val commandQueue = new MyCommandQueue(logger, batchSize = 100) {
      val succeeded = AtomicInt(0)
      protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) = {
        handleBatchSucceeded(queuedInputResponses)
        succeeded += queuedInputResponses.size
      }
      protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem) = {}
    }
    commandQueue.onCoupled(Set.empty)
    for (order <- orders) {
      commandQueue.enqueue(AgentDriver.Input.AttachOrder(order, TestAgentId))
    }
    commandQueue.maySend()
    waitForCondition(9.s, 10.ms) { commandQueue.succeeded.get() == n }
    assert(commandQueue.succeeded.get() == n)
  }
}

object CommandQueueTest {
  private val logger = Logger(getClass)
  private val TestAgentId = AgentPath("AGENT")
  private val TestWorkflow = Workflow.of(WorkflowPath("A") ~ "VERSION",
    Execute(WorkflowJob(TestAgentId, PathExecutable("EXECUTABLE"))))

  private def toQueuedInputResponse(order: Order[Order.IsFreshOrReady]) =
    QueuedInputResponse(AgentDriver.Input.AttachOrder(order, TestAgentId), Right(AgentCommand.Response.Accepted))

  private def toOrder(name: String) = Order(OrderId(name), TestWorkflow.id, Order.Fresh.StartImmediately)

  private abstract class MyCommandQueue(logger: ScalaLogger, batchSize: Int)
  extends CommandQueue(logger, batchSize = batchSize)
  {
    protected def commandParallelism = 2

    protected def executeCommand(command: AgentCommand.Batch) =
      Task(Right(Batch.Response(Vector.fill(command.commands.size)(Right(AgentCommand.Response.Accepted)))))
  }
}
