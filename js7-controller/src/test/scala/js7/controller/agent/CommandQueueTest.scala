package js7.controller.agent

import com.typesafe.scalalogging.Logger as ScalaLogger
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.Batch
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.controller.agent.AgentDriver.{Input, Queueable}
import js7.controller.agent.CommandQueue.QueuedInputResponse
import js7.controller.agent.CommandQueueTest.*
import js7.data.agent.AgentPath
import js7.data.job.PathExecutable
import js7.data.order.{Order, OrderId, OrderMark}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.execution.atomic.AtomicInt
import org.scalatest.matchers.should.Matchers.*
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class CommandQueueTest extends OurTestSuite
{
  "test" in {
    val commandQueueSucceeded = mutable.Buffer[Seq[QueuedInputResponse]]()
    val commandQueueFailed = mutable.Buffer[(Vector[Queueable], Problem)]()
    val commandQueue = new MyCommandQueue(logger, batchSize = 3) {
      protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
        commandQueueSucceeded += queuedInputResponses

      protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem) =
        commandQueueFailed += ((inputs, problem))
    }

    val expected = mutable.Buffer[Seq[QueuedInputResponse]]()

    commandQueue.onCoupled(Set.empty)

    // The first Input is sent alone to the Agent regardless of commandBatchSize.
    val aOrder = toOrder("A")
    var ok = commandQueue.enqueue(AgentDriver.Input.AttachOrder(aOrder, TestAgentPath))
    assert(ok)

    // Duplicate
    ok = commandQueue.enqueue(AgentDriver.Input.AttachOrder(aOrder, TestAgentPath))
    assert(!ok)

    expected += toQueuedInputResponse(aOrder) :: Nil
    waitForCondition(99.s, 10.ms) { commandQueueSucceeded == expected }
    assert(commandQueueSucceeded == expected)

    val twoOrders = toOrder("B") :: toOrder("C") :: Nil
    for (o <- twoOrders) commandQueue.enqueue(AgentDriver.Input.AttachOrder(o, TestAgentPath))
    waitForCondition(99.s, 10.ms) { commandQueueSucceeded == expected }
    assert(commandQueueSucceeded == expected)

    // After the Agent has processed the Input, the two queued commands are sent as a Batch to the Agent
    commandQueue.handleBatchSucceeded(commandQueueSucceeded.last) shouldEqual List(Input.AttachOrder(aOrder, TestAgentPath))
    expected += twoOrders map toQueuedInputResponse
    waitForCondition(99.s, 10.ms) { commandQueueSucceeded == expected }
    assert(commandQueueSucceeded == expected)

    val fiveOrders = toOrder("D") :: toOrder("E") :: toOrder("F") :: toOrder("G") :: toOrder("H") :: Nil
    for (o <- fiveOrders) commandQueue.enqueue(AgentDriver.Input.AttachOrder(o, TestAgentPath))
    expected += fiveOrders take 1 map toQueuedInputResponse
    waitForCondition(99.s, 10.ms) { commandQueueSucceeded == expected }
    assert(commandQueueSucceeded == expected)

    // After the Agent has processed the Input, three of the queued commands are sent as a Batch to the Agent
    commandQueue.handleBatchSucceeded(commandQueueSucceeded.last) shouldEqual fiveOrders.take(1).map(o => Input.AttachOrder(o, TestAgentPath))
    expected += fiveOrders drop 1 take 3 map toQueuedInputResponse
    waitForCondition(99.s, 10.ms) { commandQueueSucceeded == expected }
    assert(commandQueueSucceeded == expected)

    // Finally, the last queued Input is processed
    commandQueue.handleBatchSucceeded(commandQueueSucceeded.last)
    expected += fiveOrders drop 4 map toQueuedInputResponse
    waitForCondition(99.s, 10.ms) { commandQueueSucceeded == expected }
    assert(commandQueueSucceeded == expected)
    assert(commandQueueFailed.isEmpty)
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
    val commandQueueSucceeded = AtomicInt(0)
    val commandQueue = new MyCommandQueue(logger, batchSize = 100) {
      protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) = {
        handleBatchSucceeded(queuedInputResponses)
        commandQueueSucceeded += queuedInputResponses.size
      }
      protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem) = {}
    }
    commandQueue.onCoupled(Set.empty)
    for (order <- orders) {
      commandQueue.enqueue(AgentDriver.Input.AttachOrder(order, TestAgentPath))
    }
    commandQueue.maySend()
    waitForCondition(9.s, 10.ms) { commandQueueSucceeded.get() == n }
    assert(commandQueueSucceeded.get() == n)
  }
}

object CommandQueueTest {
  private val logger = Logger(getClass)
  private val TestAgentPath = AgentPath("AGENT")
  private val TestWorkflow = Workflow.of(WorkflowPath("A") ~ "VERSION",
    Execute(WorkflowJob(TestAgentPath, PathExecutable("EXECUTABLE"))))

  private def toQueuedInputResponse(order: Order[Order.IsFreshOrReady]) =
    QueuedInputResponse(AgentDriver.Input.AttachOrder(order, TestAgentPath), Right(AgentCommand.Response.Accepted))

  private def toOrder(name: String) = Order(OrderId(name), TestWorkflow.id /: Position(0), Order.Fresh)

  private abstract class MyCommandQueue(logger: ScalaLogger, batchSize: Int)
  extends CommandQueue(logger, batchSize = batchSize)
  {
    protected def commandParallelism = 2

    protected def executeCommand(command: AgentCommand.Batch) =
      Task(Right(Batch.Response(Vector.fill(command.commands.size)(Right(AgentCommand.Response.Accepted)))))
  }
}
