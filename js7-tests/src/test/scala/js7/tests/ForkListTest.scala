package js7.tests

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.configutils.Configs._
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.data.agent.AgentPath
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.job.{InternalExecutable, ShellScriptExecutable}
import js7.data.order.OrderEvent._
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, Outcome}
import js7.data.value.expression.ExpressionParser.{expr, exprFunction}
import js7.data.value.{ListValue, StringValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ForkList}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.proxy.data.event.EventAndState
import js7.tests.ForkListTest._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.Assertions.assert
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now

final class ForkListTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = yes
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(atControllerWorkflow, atAgentWorkflow, exampleWorkflow)
  private lazy val proxy = controllerApi.startProxy().await(99.s)

  "Events and Order.Forked snapshot" in {
    val workflowId = atControllerWorkflow.id
    val orderId = OrderId("SNAPSHOT-TEST")
    val n = 2

    val asserted = proxy.observable
      .collect {
        case EventAndState(Stamped(_, _, KeyedEvent(`orderId`, _: OrderForked)), _, state) =>
          assert(state.idToOrder(orderId) ==
            Order(
              orderId,
              workflowId /: Position(0),
              Order.Forked(Vector(
                Order.Forked.Child(
                  orderId | "CHILD-1",
                  Map(
                    "id" -> StringValue("CHILD-1"))),
                Order.Forked.Child(
                  orderId | "CHILD-2",
                  Map(
                    "id" -> StringValue("CHILD-2"))))),
              Map("children" -> ListValue(Seq(StringValue("CHILD-1"), StringValue("CHILD-2")))),
              deleteWhenTerminated = true))
      }
      .headL
      .runToFuture

    val eventId = eventWatch.lastAddedEventId
    controllerApi.addOrder(newOrder(orderId, workflowId.path, n)).await(99.s).orThrow

    assert(eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
      .head.value.event == OrderFinished)
    eventWatch.await[OrderDeleted](_.key == orderId, after = eventId)
    assert(eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(
        workflowId,
        Map("children" -> ListValue(Seq(StringValue("CHILD-1"), StringValue("CHILD-2")))),
        deleteWhenTerminated = true),
      OrderStarted,
      OrderForked(Vector(
        Order.Forked.Child(
          orderId | "CHILD-1",
          Map(
            "id" -> StringValue("CHILD-1"))),
        Order.Forked.Child(
          orderId | "CHILD-2",
          Map(
            "id" -> StringValue("CHILD-2"))))),
      OrderJoined(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderFinished,
      OrderDeleted))

    asserted.await(9.s)
  }

  "Fork with duplicates" in {
    val workflowId = atControllerWorkflow.id
    val orderId = OrderId("DUPLICATE-TEST")

    val eventId = eventWatch.lastAddedEventId
    val freshOrder = FreshOrder(
      orderId,
      workflowId.path,
      Map("children" -> ListValue(Seq(StringValue("DUPLICATE"), StringValue("DUPLICATE")))),
      deleteWhenTerminated = true)

    controllerApi.addOrder(freshOrder).await(99.s).orThrow

    eventWatch.await[OrderFailed](_.key == orderId, after = eventId)
    assert(eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(
        workflowId,
        Map("children" -> ListValue(Seq(StringValue("DUPLICATE"), StringValue("DUPLICATE")))),
        deleteWhenTerminated = true),
      OrderStarted,
      OrderFailed(
        Position(0),
        Some(Outcome.Disrupted(Problem("Duplicate fork values in $children: Unexpected duplicates: 2×DUPLICATE"))))))
  }

  "Fork with invalid value" in {
    val childToProblem = Seq(
      "|" -> Problem("Order ChildId must not contain '|'"),
      "" -> EmptyStringProblem("OrderId.Child"))
    for (((childId, problem), i) <- childToProblem.zipWithIndex) {
      val workflowId = atControllerWorkflow.id
      val orderId = OrderId(s"INVALID-TEST-$i")

      val eventId = eventWatch.lastAddedEventId
      val freshOrder = FreshOrder(
        orderId,
        workflowId.path,
        Map("children" -> ListValue(Seq(StringValue(childId)))),
        deleteWhenTerminated = true)

      controllerApi.addOrder(freshOrder).await(99.s).orThrow

      eventWatch.await[OrderFailed](_.key == orderId, after = eventId)
      assert(eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
        OrderAdded(
          workflowId,
          Map("children" -> ListValue(Seq(StringValue(childId)))),
          deleteWhenTerminated = true),
        OrderStarted,
        OrderFailed(
          Position(0),
          Some(Outcome.Disrupted(problem)))))
    }
  }

  "Fork with empty array" in {
    runOrder(atControllerWorkflow.path, OrderId("EMPTY"), n = 0)
    runOrder(atAgentWorkflow.path, OrderId("EMPTY"), n = 0)
  }

  "Fork with a small array" in {
    // Use same OrderId twice to test the ForkInstructionExecutor.Cache.
    val orderId = OrderId("SMALL")
    runOrder(atControllerWorkflow.path, orderId, n = 3)
    runOrder(atAgentWorkflow.path, orderId, n = 3)
  }

  "Fork with big array" in {
    val n = sys.props.get("test.speed").fold(1000)(_.toInt)
    val t = now
    runOrder(atAgentWorkflow.path, OrderId("BIG"), n)
    logger.info(itemsPerSecondString(t.elapsed, n, "orders"))
  }

  private def runOrder(workflowPath: WorkflowPath, orderId: OrderId, n: Int): Unit = {
    val childIds = for (i <- 1 to n) yield s"CHILD-$i"
    val childOrderIds = for (id <- childIds) yield orderId | id
    val eventId = eventWatch.lastAddedEventId

    val childOrdersProcessed = proxy.observable
      .map(_.stampedEvent.value)
      .collect { case KeyedEvent(orderId: OrderId, _: OrderProcessed) => orderId }
      .scan0(childOrderIds.toSet)(_ - _)
      .takeWhile(_.nonEmpty)
      .completedL
      .runToFuture

    val order = newOrder(orderId, workflowPath, n)
    controllerApi.addOrders(Observable(order)).await(99.s).orThrow

    assert(eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
      .head.value.event == OrderFinished)

    childOrdersProcessed.await(9.s)
  }

  "Example with a simple script" in {
    logger.debug(exampleWorkflow.asJson.compactPrint)
    val workflowId = exampleWorkflow.id
    val orderId = OrderId(s"EXAMPLE")
    val children = for (i <- 1 to 3) yield s"CHILD-$i"

    val eventId = eventWatch.lastAddedEventId
    val freshOrder = FreshOrder(
      orderId,
      workflowId.path,
      Map("children" -> ListValue(children.map(StringValue(_)))),
      deleteWhenTerminated = true)

    controllerApi.addOrder(freshOrder).await(99.s).orThrow

    assert(eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
      .head.value.event == OrderFinished)
    for (childId <- children) {
      assert(eventWatch.keyedEvents[OrderEvent](orderId | childId) == Seq(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted,
        OrderStdoutWritten(s"CHILD_ID=$childId\n"),
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(0) / "fork" % 1),
        OrderDetachable,
        OrderDetached))
    }
  }
}

object ForkListTest
{
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")

  private val forkList = ForkList(
    expr("$children"),
    exprFunction("(id) => { id: $id }"),
    Workflow.of(Execute(WorkflowJob(
      agentPath,
      InternalExecutable(classOf[TestJob].getName),
      parallelism = 100_000))))

  private val atControllerWorkflow = Workflow(
    WorkflowPath("AT-CONTROLLER-WORKFLOW") ~ "INITIAL",
    Vector(
      forkList/*Fork at controller*/))

  private val atAgentWorkflow = Workflow(
    WorkflowPath("AT-AGENT-WORKFLOW") ~ "INITIAL",
    Vector(
      EmptyJob.execute(agentPath),
      forkList/*Fork at agent*/))

  final class TestJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess(Task {
        assert(step.order.arguments.keySet == Set("id", "children"))
        assert(step.order.arguments("id").toStringValueString.orThrow.startsWith("CHILD-"))
        step.order.arguments("children").asListValue.orThrow
        Outcome.succeeded
      })
  }

  private val exampleWorkflow = Workflow(
    WorkflowPath("EXAMPLE") ~ "INITIAL",
    Vector(
      ForkList(
          expr("$children"),
          exprFunction("(x) => { childId: $x }"),
          Workflow.of(
            Execute(WorkflowJob(
              agentPath,
              ShellScriptExecutable(
               """#!/usr/bin/env bash
                 |set -euo pipefail
                 |echo CHILD_ID=$CHILD_ID
                 |""".stripMargin,
                env = Map(
                  "CHILD_ID" -> expr("$childId")))))))))

  private def newOrder(orderId: OrderId, workflowPath: WorkflowPath, n: Int) =
    FreshOrder(
      orderId,
      workflowPath,
      Map("children" -> ListValue(for (i <- 1 to n) yield StringValue(s"CHILD-$i"))),
      deleteWhenTerminated = true)
}
