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
import js7.data.workflow.instructions.{Execute, ForkList, If}
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

  protected val agentPaths = Seq(agentPath, bAgentPath)
  protected val items = Seq(atControllerWorkflow, atAgentWorkflow, mixedAgentsWorkflow,
    errorWorkflow, exampleWorkflow)
  private lazy val proxy = controllerApi.startProxy().await(99.s)

  "Events and Order.Forked snapshot" in {
    val workflowId = atControllerWorkflow.id
    val orderId = OrderId("SNAPSHOT-TEST")
    val n = 3

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
                    "id" -> StringValue("CHILD-2"))),
                Order.Forked.Child(
                  orderId | "CHILD-3",
                  Map(
                    "id" -> StringValue("CHILD-3"))))),
              Map("children" -> ListValue(Seq(
                StringValue("CHILD-1"),
                StringValue("CHILD-2"),
                StringValue("CHILD-3")))),
              attachedState = Some(Order.Attached(agentPath)),
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
        Map("children" -> ListValue(Seq(
          StringValue("CHILD-1"),
          StringValue("CHILD-2"),
          StringValue("CHILD-3")))),
        deleteWhenTerminated = true),
      OrderStarted,
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderForked(Vector(
        Order.Forked.Child(
          orderId | "CHILD-1",
          Map(
            "id" -> StringValue("CHILD-1"))),
        Order.Forked.Child(
          orderId | "CHILD-2",
          Map(
            "id" -> StringValue("CHILD-2"))),
        Order.Forked.Child(
          orderId | "CHILD-3",
          Map(
            "id" -> StringValue("CHILD-3"))))),
      OrderDetachable,
      OrderDetached,
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
    val n = sys.props.get("test.speed").fold(100)(_.toInt)
    val t = now
    runOrder(atControllerWorkflow.path, OrderId("BIG"), n)
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

  "Mixed agents" in {
    val workflowId = mixedAgentsWorkflow.id
    val orderId = OrderId(s"MIXED")
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
    eventWatch.await[OrderDeleted](_.key == orderId, after = eventId)

    val orderForked = OrderForked(
      children
        .map(child => Order.Forked.Child(orderId | child, Map("id" -> StringValue(child))))
        .toVector)
    assert(eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(
        workflowId,
        Map("children" -> ListValue(children.map(StringValue(_)))),
        deleteWhenTerminated = true),

      // Each child order starts at bAgentPath. So attach forking order to bAgentPath.
      OrderStarted,
      OrderAttachable(bAgentPath),
      OrderAttached(bAgentPath),
      orderForked,
      OrderDetachable,
      OrderDetached,
      OrderJoined(Outcome.succeeded),
      OrderMoved(Position(1)),

      // Each child order starts at a different Agent. So let forking order detached.
      orderForked,
      OrderJoined(Outcome.succeeded),
      OrderMoved(Position(2)),

      // Attach to bAgentPath
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(3)),

      // Each child order starts at a different Agent. So detach forking order (should we?)
      OrderDetachable,
      OrderDetached,
      orderForked,
      OrderJoined(Outcome.succeeded),
      OrderMoved(Position(4)),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(5)),
      // Each child order starts at bAgentPath. So detach and then attach forking order to bAgentPath.
      OrderDetachable,
      OrderDetached,
      OrderAttachable(bAgentPath),
      OrderAttached(bAgentPath),
      orderForked,
      OrderDetachable,
      OrderDetached,
      OrderJoined(Outcome.succeeded),
      OrderMoved(Position(6)),
      OrderFinished,
      OrderDeleted))
  }

  "ForkList containing a first failing If statement failed before forking" in {
    val workflowId = errorWorkflow.id
    val orderId = OrderId(s"ERROR")

    val eventId = eventWatch.lastAddedEventId
    val freshOrder = FreshOrder(
      orderId,
      workflowId.path,
      Map("children" -> ListValue(Seq(StringValue("CHILD")))),
      deleteWhenTerminated = true)
    controllerApi.addOrder(freshOrder).await(99.s).orThrow

    eventWatch.await[OrderFailed](_.key == orderId, after = eventId)

    assert(eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(
        workflowId,
        Map("children" -> ListValue(Seq(StringValue("CHILD")))),
        deleteWhenTerminated = true),
      OrderStarted,
      OrderFailed(Position(0), Some(Outcome.Failed(Some("No such named value: UNKNOWN"))))))
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
  private val bAgentPath = AgentPath("B-AGENT")

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

  private val mixedAgentsWorkflow = Workflow(
    WorkflowPath("MIXED-WORKFLOW") ~ "INITIAL",
    Vector(
      // Each child order starts at bAgentPath. So attach forking order to bAgentPath.
      ForkList(
        expr("$children"),
        exprFunction("(id) => { id: $id }"),
        Workflow.of(
          If(
            expr("$id == 'CHILD-X'"),
            Workflow.of(EmptyJob.execute(agentPath)),
            Some(Workflow.of(EmptyJob.execute(bAgentPath)))))),

      // Each child order starts at a different Agent. So let forking order detached.
      ForkList(
        expr("$children"),
        exprFunction("(id) => { id: $id }"),
        Workflow.of(
          If(
            expr("$id == 'CHILD-1'"),
            Workflow.of(EmptyJob.execute(agentPath)),
            Some(Workflow.of(EmptyJob.execute(bAgentPath)))))),

      EmptyJob.execute(agentPath),
      // Each child order starts at a different Agent. So detach forking order.
      ForkList(
        expr("$children"),
        exprFunction("(id) => { id: $id }"),
        Workflow.of(
          If(
            expr("$id == 'CHILD-1'"),
            Workflow.of(EmptyJob.execute(agentPath)),
            Some(Workflow.of(EmptyJob.execute(bAgentPath)))))),

      EmptyJob.execute(agentPath),
      // Each child order starts at bAgentPath. So detach and then attach forking order to bAgentPath.
      ForkList(
        expr("$children"),
        exprFunction("(id) => { id: $id }"),
        Workflow.of(
          If(
            expr("$id == 'CHILD-X'"),
            Workflow.of(EmptyJob.execute(agentPath)),
            Some(Workflow.of(EmptyJob.execute(bAgentPath))))))))

  private val errorWorkflow = Workflow(
    WorkflowPath("ERROR-WORKFLOW") ~ "INITIAL",
    Vector(
      ForkList(
        expr("$children"),
        exprFunction("(id) => { id: $id }"),
        Workflow.of(
          If(
            expr("$UNKNOWN == 'UNKNOWN'"),
            Workflow.of(EmptyJob.execute(agentPath)))))))

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
