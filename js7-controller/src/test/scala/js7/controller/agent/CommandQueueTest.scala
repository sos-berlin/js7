package js7.controller.agent

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.traverse.*
import com.typesafe.scalalogging.Logger as ScalaLogger
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.Batch
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.controller.agent.AgentDriver.Queueable
import js7.controller.agent.CommandQueue.QueueableResponse
import js7.controller.agent.CommandQueueTest.*
import js7.data.agent.AgentPath
import js7.data.job.PathExecutable
import js7.data.order.{Order, OrderId, OrderMark}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.ScalaTestUtils.awaitAndAssert
import org.scalatest.matchers.should.Matchers.*
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class CommandQueueTest extends OurTestSuite:

  private given IORuntime = ioRuntime

  "test" in:
    val commandQueueSucceeded = mutable.Buffer[Seq[QueueableResponse]]()
    val commandQueueFailed = mutable.Buffer[(Vector[Queueable], Problem)]()
    val commandQueue = new MyCommandQueue(logger, batchSize = 3):
      protected def asyncOnBatchSucceeded(queueableResponses: Seq[QueueableResponse]) =
        IO(commandQueueSucceeded += queueableResponses)

      protected def asyncOnBatchFailed(queueables: Vector[Queueable], problem: Problem) =
        IO(commandQueueFailed += ((queueables, problem)))

    val expected = mutable.Buffer[Seq[QueueableResponse]]()

    commandQueue.onCoupled(Set.empty).unsafeRunSync()

    // The first Queueable is sent alone to the Agent regardless of commandBatchSize.
    val aOrder = toOrder("A")
    var ok = commandQueue.enqueue(AgentDriver.Queueable.AttachOrder(aOrder, TestAgentPath))
      .unsafeRunSync()
    assert(ok)

    // Duplicate
    ok = commandQueue.enqueue(AgentDriver.Queueable.AttachOrder(aOrder, TestAgentPath))
      .unsafeRunSync()
    assert(!ok)

    expected += toQueuedInputResponse(aOrder) :: Nil
    awaitAndAssert { commandQueueSucceeded == expected }
    assert(commandQueueSucceeded == expected)

    val twoOrders = toOrder("B") :: toOrder("C") :: Nil
    for o <- twoOrders do commandQueue.enqueue(AgentDriver.Queueable.AttachOrder(o, TestAgentPath))
      .unsafeRunSync()
    awaitAndAssert { commandQueueSucceeded == expected }

    // After the Agent has processed the Queueable, the two queued commands are sent as a Batch to the Agent
    commandQueue.handleBatchSucceeded(commandQueueSucceeded.last).unsafeRunSync() shouldEqual List(Queueable.AttachOrder(aOrder, TestAgentPath))
    expected += twoOrders map toQueuedInputResponse
    awaitAndAssert { commandQueueSucceeded == expected }

    val fiveOrders = toOrder("D") :: toOrder("E") :: toOrder("F") :: toOrder("G") :: toOrder("H") :: Nil
    for o <- fiveOrders do commandQueue.enqueue(AgentDriver.Queueable.AttachOrder(o, TestAgentPath))
      .unsafeRunSync()
    expected += fiveOrders take 1 map toQueuedInputResponse
    awaitAndAssert { commandQueueSucceeded == expected }

    // After the Agent has processed the Queueable, three of the queued commands are sent as a Batch to the Agent
    commandQueue.handleBatchSucceeded(commandQueueSucceeded.last).unsafeRunSync() shouldEqual fiveOrders.take(1).map(o => Queueable.AttachOrder(o, TestAgentPath))
    expected += fiveOrders drop 1 take 3 map toQueuedInputResponse
    awaitAndAssert { commandQueueSucceeded == expected }

    // Finally, the last queued Queueable is processed
    commandQueue.handleBatchSucceeded(commandQueueSucceeded.last).unsafeRunSync()
    expected += fiveOrders drop 4 map toQueuedInputResponse
    awaitAndAssert { commandQueueSucceeded == expected }
    assert(commandQueueFailed.isEmpty)

  "Duplicate MarkOrder" in:
    // ControllerOrderQueue may send MarkOrder for each OrderEvent received from Agent
    // because MarkOrder is executeted asynchronously and effect occurs later.
    val queue = new MyCommandQueue(logger, batchSize = 3):
      protected def asyncOnBatchSucceeded(queueableResponses: Seq[QueueableResponse]) =
        IO.unit

      protected def asyncOnBatchFailed(queueables: Vector[Queueable], problem: Problem) =
        IO.unit
    var ok = queue.enqueue(Queueable.MarkOrder(OrderId("ORDER"), OrderMark.Suspending())).await(99.s)
    assert(ok)
    ok = queue.enqueue(Queueable.MarkOrder(OrderId("ORDER"), OrderMark.Suspending())).await(99.s)
    assert(!ok)
    ok = queue.enqueue(Queueable.MarkOrder(OrderId("ORDER"), OrderMark.Resuming())).await(99.s)
    assert(ok)

  "Performance with any orders" in:
    // Run with -DCommandQueueTest=10000000 (10 million) -Xmx3g
    val n = sys.props.get("CommandQueueTest").map(_.toInt) getOrElse 10000
    val commandQueueSucceeded = Atomic(0)

    val commandQueue = new MyCommandQueue(logger, batchSize = 100):
      protected def asyncOnBatchSucceeded(queueableResponses: Seq[QueueableResponse]) =
        commandQueueSucceeded += queueableResponses.size
        handleBatchSucceeded(queueableResponses).void
      protected def asyncOnBatchFailed(queueables: Vector[Queueable], problem: Problem) =
        fail(problem.toString)

    commandQueue.onCoupled(Set.empty).await(99.s)
    (1 to n).view
      .map(i => toOrder(i.toString))
      .to(ArraySeq)
      .traverse(order => commandQueue.enqueue(AgentDriver.Queueable.AttachOrder(order, TestAgentPath)))
      .await(99.s)
    commandQueue.maybeStartSending.await(99.s)
    awaitAndAssert:
      commandQueueSucceeded.get() == n


object CommandQueueTest:
  private val logger = Logger[this.type]
  private val TestAgentPath = AgentPath("AGENT")
  private val TestWorkflow = Workflow.of(WorkflowPath("A") ~ "VERSION",
    Execute(WorkflowJob(TestAgentPath, PathExecutable("EXECUTABLE"))))

  private def toQueuedInputResponse(order: Order[Order.IsFreshOrReady]) =
    QueueableResponse(AgentDriver.Queueable.AttachOrder(order, TestAgentPath), Right(AgentCommand.Response.Accepted))

  private def toOrder(name: String) = Order(OrderId(name), TestWorkflow.id /: Position(0), Order.Fresh())

  private abstract class MyCommandQueue(logger: ScalaLogger, batchSize: Int)
  extends CommandQueue(TestAgentPath, batchSize = batchSize, commandErrorDelay = 1.s):
    protected def commandParallelism = 2

    protected def executeCommand(command: AgentCommand.Batch) =
      IO(Right(Batch.Response(Vector.fill(command.commands.size)(Right(AgentCommand.Response.Accepted)))))
