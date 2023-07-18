package js7.tests

import io.circe.syntax.EncoderOps
import java.lang.System.lineSeparator as nl
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.configutils.Configs.*
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.execution.workflow.instructions.ForkInstructionExecutor
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.*
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, Outcome}
import js7.data.subagent.{SubagentSelection, SubagentSelectionId}
import js7.data.value.expression.Expression.{ListExpression, NumericConstant}
import js7.data.value.expression.ExpressionParser.{expr, exprFunction}
import js7.data.value.{ListValue, NumberValue, ObjectValue, StringValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fail, Fork, ForkList, If}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.proxy.data.event.EventAndState
import js7.tests.ForkListTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import org.scalatest.Assertions.assert
import scala.collection.View
import scala.concurrent.duration.Deadline.now

final class ForkListTest extends OurTestSuite with ControllerAgentForScalaTest
with BlockingItemUpdater
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = yes
    """

  protected val agentPaths = Seq(agentPath, bAgentPath)
  protected val items = Seq(atControllerWorkflow, atAgentWorkflow, mixedAgentsWorkflow,
    errorWorkflow, failingChildOrdersWorkflow, joinFailingChildOrdersWorkflow,
    indexWorkflow, exampleWorkflow)
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
                  orderId / "ELEMENT-1",
                  Map(
                    "element" -> StringValue("ELEMENT-1"))),
                Order.Forked.Child(
                  orderId / "ELEMENT-2",
                  Map(
                    "element" -> StringValue("ELEMENT-2"))),
                Order.Forked.Child(
                  orderId / "ELEMENT-3",
                  Map(
                    "element" -> StringValue("ELEMENT-3"))))),
              Map("myList" -> ListValue(Seq(
                StringValue("ELEMENT-1"),
                StringValue("ELEMENT-2"),
                StringValue("ELEMENT-3")))),
              attachedState = Some(Order.Attached(agentPath)),
              deleteWhenTerminated = true))
      }
      .headL
      .runToFuture

    val eventId = eventWatch.lastAddedEventId
    controllerApi
      .addOrder(newOrder(orderId, workflowId.path, n, deleteWhenTerminated = true))
      .await(99.s).orThrow

    assert(eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
      .head.value.event == OrderFinished())
    eventWatch.await[OrderDeleted](_.key == orderId, after = eventId)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(
        workflowId,
        Map("myList" -> ListValue(Seq(
          StringValue("ELEMENT-1"),
          StringValue("ELEMENT-2"),
          StringValue("ELEMENT-3")))),
        deleteWhenTerminated = true),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderForked(Vector(
        Order.Forked.Child(
          orderId / "ELEMENT-1",
          Map(
            "element" -> StringValue("ELEMENT-1"))),
        Order.Forked.Child(
          orderId / "ELEMENT-2",
          Map(
            "element" -> StringValue("ELEMENT-2"))),
        Order.Forked.Child(
          orderId / "ELEMENT-3",
          Map(
            "element" -> StringValue("ELEMENT-3"))))),
      OrderDetachable,
      OrderDetached,
      OrderJoined(Outcome.Succeeded(Map("resultList" -> ListValue(Seq(
        StringValue("🔹" + (orderId / "ELEMENT-1").string),
        StringValue("🔹" + (orderId / "ELEMENT-2").string),
        StringValue("🔹" + (orderId / "ELEMENT-3").string)))))),
      OrderMoved(Position(1)),
      OrderFinished(),
      OrderDeleted))

    asserted.await(9.s)
  }

  "ForkList with duplicates" in {
    val workflowId = atControllerWorkflow.id
    val orderId = OrderId("DUPLICATE-TEST")

    val eventId = eventWatch.lastAddedEventId
    val freshOrder = FreshOrder(
      orderId,
      workflowId.path,
      Map("myList" -> ListValue(Seq(StringValue("DUPLICATE"), StringValue("DUPLICATE")))),
      deleteWhenTerminated = true)

    controllerApi.addOrder(freshOrder).await(99.s).orThrow

    eventWatch.await[OrderFailed](_.key == orderId, after = eventId)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(
        workflowId,
        Map("myList" -> ListValue(Seq(StringValue("DUPLICATE"), StringValue("DUPLICATE")))),
        deleteWhenTerminated = true),
      //OrderStarted,
      OrderOutcomeAdded(Outcome.Disrupted(Problem(
        "Duplicate child IDs in $myList: Unexpected duplicates: 2×DUPLICATE"))),
      OrderFailed(Position(0))))
  }

  "ForkList with invalid value" in {
    val childToProblem = Seq(
      "|" -> Problem("OrderId must not contain reserved characters: |"),
      "" -> EmptyStringProblem("OrderId.withChild"))
    for (((childId, problem), i) <- childToProblem.zipWithIndex) {
      val workflowId = atControllerWorkflow.id
      val orderId = OrderId(s"INVALID-TEST-$i")

      val eventId = eventWatch.lastAddedEventId
      val freshOrder = FreshOrder(
        orderId,
        workflowId.path,
        Map("myList" -> ListValue(Seq(StringValue(childId)))),
        deleteWhenTerminated = true)

      controllerApi.addOrder(freshOrder).await(99.s).orThrow

      eventWatch.await[OrderFailed](_.key == orderId, after = eventId)
      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(
          workflowId,
          Map("myList" -> ListValue(Seq(StringValue(childId)))),
          deleteWhenTerminated = true),
        //OrderStarted,
        OrderOutcomeAdded(Outcome.Disrupted(problem)),
        OrderFailed(Position(0))))
    }
  }

  "ForkList with index" in {
    val workflowId = indexWorkflow.id
    val orderId = OrderId(s"INDEX")

    val myList = ListValue(for (i <- Seq("A", "B", "C")) yield StringValue(i))
    val freshOrder = FreshOrder(orderId, workflowId.path, Map("myList" -> myList),
      deleteWhenTerminated = true)

    val eventId = eventWatch.lastAddedEventId
    controllerApi.addOrder(freshOrder).await(99.s).orThrow

    assert(eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
      .head.value.event == OrderFinished())
    eventWatch.await[OrderDeleted](_.key == orderId, after = eventId)
    for (i <- myList.elements.indices) {
      assert(eventWatch.eventsByKey[OrderEvent](orderId / i.toString) == Seq(
        OrderMoved(Position(0) / "fork" % 1),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(0) / "fork" % 2),
        OrderDetachable,
        OrderDetached))
    }
  }

  "ForkList with empty array" in {
    runOrder(atControllerWorkflow.path, OrderId("EMPTY"), n = 0)
    runOrder(atAgentWorkflow.path, OrderId("EMPTY"), n = 0)
  }

  "ForkList with single element" in {
    runOrder(atControllerWorkflow.path, OrderId("EMPTY"), n = 1)
    runOrder(atAgentWorkflow.path, OrderId("EMPTY"), n = 1)
  }

  "ForkList with a small array" in {
    // Use same OrderId twice to test the ForkInstructionExecutor.Cache.
    val orderId = OrderId("SMALL")
    runOrder(atControllerWorkflow.path, orderId, n = 3)
    runOrder(atAgentWorkflow.path, orderId, n = 3)
  }

  "ForkList with big array" in {
    val n = sys.props.get("test.speed").fold(100)(_.toInt)
    val t = now
    runOrder(atControllerWorkflow.path, OrderId("BIG"), n)
    logger.info(itemsPerSecondString(t.elapsed, n, "orders"))
  }

  "ForkList with failing child orders, joinIfFailed=true" in {
    val order = runOrder(joinFailingChildOrdersWorkflow.path, OrderId("FAIL-THEN-JOIN"), 2,
      expectedChildOrderEvent = _ => OrderFailedInFork(Position(0) / "fork" % 1),
      expectedTerminationEvent = OrderFailed(Position(0)))

    assert(order.lastOutcome == Outcome.Failed(Some(
      """Order:FAIL-THEN-JOIN|ELEMENT-1 Failed(TEST FAILURE);
        |Order:FAIL-THEN-JOIN|ELEMENT-2 Failed(TEST FAILURE)"""
        .stripMargin)))
  }

  "ForkList with failing child orders" in {
    val order = runOrder(failingChildOrdersWorkflow.path, OrderId("FAIL-THEN-STOP"), 2,
      expectedChildOrderEvent = _ => OrderFailed(Position(0) / "fork" % 1),
      cancelChildOrders = true,
      expectedTerminationEvent = OrderFailed(Position(0)))

    assert(order.lastOutcome == Outcome.Failed(Some(
      """Order:FAIL-THEN-STOP|ELEMENT-1 has been cancelled;
        |Order:FAIL-THEN-STOP|ELEMENT-2 has been cancelled"""
        .stripMargin)))
  }

  "ForkList with nested Fork" in {
    // This should not be a special problem
    val subagentSelection = SubagentSelection(
      SubagentSelectionId("active-active-all-agents"),
      Map(subagentId -> 1))
    updateItem(subagentSelection)
    val list = (1 to ForkInstructionExecutor.MinimumChildCountForParentAttachment).toList
    val listExpr = ListExpression(list.map(NumericConstant(_)))

    val workflow = updateItem(Workflow.of(
      WorkflowPath("FORK-IN-FORKLIST"),
      EmptyJob.execute(agentPath),
      ForkList(
        listExpr,
        exprFunction("(element) => $element"),
        exprFunction("(element) => { element: $element }"),
        agentPath = Some(agentPath),
        workflow = Workflow.of(
          Fork(Vector.empty)))))

    try {
      val eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("FORK-IN-FORKLIST")
      val events = controller.runOrder(FreshOrder(orderId, workflow.path, Map(
        "myList" -> ListValue(Seq(StringValue("VALUE"))))))

      assert(events.map(_.value) == Seq(
        OrderAdded(workflow.id, Map("myList" -> ListValue(Seq(StringValue("VALUE"))))),

        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(Some(subagentId)),
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(1)),

        OrderForked(Vector(
          OrderForked.Child(orderId / "1", Map("element" -> NumberValue(1))),
          OrderForked.Child(orderId / "2", Map("element" -> NumberValue(2))),
          OrderForked.Child(orderId / "3", Map("element" -> NumberValue(3))))),
        OrderDetachable,
        OrderDetached,

        OrderJoined(Outcome.succeeded),
        OrderMoved(Position(2)),

        OrderFinished(None)))

      for (i <- list) withClue(s"i=$i ") {
        assert(eventWatch
          .keyedEvents[OrderEvent](_.key == orderId / s"$i", after = eventId)
          .map(_.event) ==
          Seq(
            OrderDetachable,
            OrderDetached,
            OrderForked(Vector.empty),
            OrderJoined(Outcome.succeeded),
            OrderMoved(Position(1) / "fork" % 1)))
      }
    } finally
      deleteItems(workflow.path, subagentSelection.path)
  }

  private def runOrder(workflowPath: WorkflowPath, orderId: OrderId, n: Int,
    expectedChildOrderEvent: OrderId => OrderEvent =
      orderId => OrderProcessed(Outcome.Succeeded(Map("result" -> StringValue("🔹" + orderId.string)))),
    expectedTerminationEvent: OrderTerminated = OrderFinished(),
    cancelChildOrders: Boolean = false)
  : Order[Order.State] = {
    val childOrderIds = (1 to n).map(i => orderId / s"ELEMENT-$i").toSet
    val eventId = eventWatch.lastAddedEventId

    val childOrdersProcessed = proxy.observable
      .map(_.stampedEvent.value)
      .collect {
        case KeyedEvent(orderId: OrderId, event: OrderEvent)
        if event == expectedChildOrderEvent(orderId) =>
          orderId
      }
      .scan0(childOrderIds)(_ - _)
      .takeWhile(_.nonEmpty)
      .completedL
      .runToFuture

    val order = newOrder(orderId, workflowPath, n)
    controllerApi.addOrders(Observable(order)).await(99.s).orThrow

    childOrdersProcessed.await(9.s)
    if (cancelChildOrders) {
      controllerApi.executeCommand(
        CancelOrders(childOrderIds)
      ).await(99.s).orThrow
    }

    assert(eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
      .head.value.event == expectedTerminationEvent)
    val terminatedOrder = controllerState.idToOrder(orderId)
    controllerApi.deleteOrdersWhenTerminated(Observable(orderId)).await(99.s).orThrow
    terminatedOrder
  }

  "Mixed agents" in {
    val workflowId = mixedAgentsWorkflow.id
    val orderId = OrderId("MIXED")
    val myList = for (i <- 1 to 3) yield s"ELEMENT-$i"

    val eventId = eventWatch.lastAddedEventId
    val freshOrder = FreshOrder(
      orderId,
      workflowId.path,
      Map("myList" -> ListValue(myList.map(StringValue(_)))),
      deleteWhenTerminated = true)
    controllerApi.addOrder(freshOrder).await(99.s).orThrow

    assert(eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
      .head.value.event == OrderFinished())
    eventWatch.await[OrderDeleted](_.key == orderId, after = eventId)

    val orderForked = OrderForked(
      myList
        .map(child => Order.Forked.Child(orderId / child, Map("element" -> StringValue(child))))
        .toVector)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(
        workflowId,
        Map("myList" -> ListValue(myList.map(StringValue(_)))),
        deleteWhenTerminated = true),

      // Each child order starts at bAgentPath. So attach forking order to bAgentPath.
      OrderAttachable(bAgentPath),
      OrderAttached(bAgentPath),
      OrderStarted,
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
      OrderProcessingStarted(subagentId),
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
      OrderProcessingStarted(subagentId),
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
      OrderFinished(),
      OrderDeleted))
  }

  "ForkList containing a first failing If statement failed before forking" in {
    // This is due to the prediction, at which agent the child orders will starts
    val workflowId = errorWorkflow.id
    val orderId = OrderId(s"ERROR")

    val eventId = eventWatch.lastAddedEventId
    val freshOrder = FreshOrder(
      orderId,
      workflowId.path,
      Map("myList" -> ListValue(Seq(StringValue("SINGLE-ELEMENT")))),
      deleteWhenTerminated = true)
    controllerApi.addOrder(freshOrder).await(99.s).orThrow

    eventWatch.await[OrderFailed](_.key == orderId, after = eventId)

    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(
        workflowId,
        Map("myList" -> ListValue(Seq(StringValue("SINGLE-ELEMENT")))),
        deleteWhenTerminated = true),
      //Until v2.5.0: OrderStarted,
      //Until v2.5.0: OrderOutcomeAdded(Outcome.Failed(Some("No such named value: UNKNOWN"))),
      OrderOutcomeAdded(Outcome.Disrupted(Problem("No such named value: UNKNOWN"))),
      OrderFailed(Position(0))))
  }

  "Example with a simple script" in {
    logger.debug(exampleWorkflow.asJson.compactPrint)
    val workflowId = exampleWorkflow.id
    val orderId = OrderId("EXAMPLE")
    val myList = ListValue(Seq(
      ObjectValue(Map(
        "id" -> NumberValue(1),
        "content" -> StringValue("EINS"))),
      ObjectValue(Map(
        "id" -> NumberValue(2),
        "content" -> StringValue("ZWEI"))),
      ObjectValue(Map(
        "id" -> NumberValue(3),
        "content" -> StringValue("DREI")))))

    val eventId = eventWatch.lastAddedEventId
    val freshOrder = FreshOrder(
      orderId,
      workflowId.path,
      Map("myList" -> myList),
      deleteWhenTerminated = true)
    controllerApi.addOrder(freshOrder).await(99.s).orThrow

    assert(eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
      .head.value.event == OrderFinished())
    for (elementId <- View("1", "2", "3")) {
      assert(eventWatch.eventsByKey[OrderEvent](orderId / elementId) == Seq(
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten(s"ELEMENT_ID=$elementId$nl"),
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
  private val subagentId = toLocalSubagentId(agentPath)
  private val bAgentPath = AgentPath("B-AGENT")

  private val forkList = ForkList(
    expr("$myList"),
    exprFunction("(element) => $element"),
    exprFunction("(element) => { element: $element }"),
    Workflow.anonymous(
      Seq(
        TestJob.execute(agentPath, parallelism = 100_000)),
      result = Some(Map("resultList" -> expr("$result")))))

  private val atControllerWorkflow = Workflow(
    WorkflowPath("AT-CONTROLLER-WORKFLOW") ~ "INITIAL",
    Vector(
      forkList/*Fork at controller*/))

  private val atAgentWorkflow = Workflow(
    WorkflowPath("AT-AGENT-WORKFLOW") ~ "INITIAL",
    Vector(
      EmptyJob.execute(agentPath),
      forkList/*Fork at agent*/))

  private class TestJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess(Task {
        assert(step.order.arguments.keySet == Set("element", "myList"))
        assert(step.order.arguments("element").toStringValueString.orThrow.startsWith("ELEMENT-"))
        step.order.arguments("myList").as[ListValue].orThrow
        Outcome.Succeeded(Map("result" -> StringValue("🔹" + step.order.id.string)))
      })
  }
  private object TestJob extends InternalJob.Companion[TestJob]

  private val mixedAgentsWorkflow = Workflow(
    WorkflowPath("MIXED-WORKFLOW") ~ "INITIAL",
    Vector(
      // Each child order starts at bAgentPath. So attach forking order to bAgentPath.
      ForkList(
        expr("$myList"),
        exprFunction("(element) => $element"),
        exprFunction("(element) => { element: $element }"),
        Workflow.of(
          If(
            expr("$element == 'ELEMENT-X'"),
            Workflow.of(EmptyJob.execute(agentPath)),
            Some(Workflow.of(EmptyJob.execute(bAgentPath)))))),

      // Each child order starts at a different Agent. So let forking order detached.
      ForkList(
        expr("$myList"),
        exprFunction("(element) => $element"),
        exprFunction("(element) => { element: $element }"),
        Workflow.of(
          If(
            expr("$element == 'ELEMENT-1'"),
            Workflow.of(EmptyJob.execute(agentPath)),
            Some(Workflow.of(EmptyJob.execute(bAgentPath)))))),

      EmptyJob.execute(agentPath),
      // Each child order starts at a different Agent. So detach forking order.
      ForkList(
        expr("$myList"),
        exprFunction("(element) => $element"),
        exprFunction("(element) => { element: $element }"),
        Workflow.of(
          If(
            expr("$element == 'ELEMENT-1'"),
            Workflow.of(EmptyJob.execute(agentPath)),
            Some(Workflow.of(EmptyJob.execute(bAgentPath)))))),

      EmptyJob.execute(agentPath),
      // Each child order starts at bAgentPath. So detach and then attach forking order to bAgentPath.
      ForkList(
        expr("$myList"),
        exprFunction("(element) => $element"),
        exprFunction("(element) => { element: $element }"),
        Workflow.of(
          If(
            expr("$element == 'ELEMENT-X'"),
            Workflow.of(EmptyJob.execute(agentPath)),
            Some(Workflow.of(EmptyJob.execute(bAgentPath))))))))

  private val errorWorkflow = Workflow(
    WorkflowPath("ERROR-WORKFLOW") ~ "INITIAL",
    Vector(
      ForkList(
        expr("$myList"),
        exprFunction("(element) => $element"),
        exprFunction("(element) => { element: $element }"),
        Workflow.of(
          If(
            expr("$UNKNOWN == 'UNKNOWN'"),
            Workflow.of(EmptyJob.execute(agentPath)))))))

  private val failingChildOrdersWorkflow = Workflow(
    WorkflowPath("FAILING-CHILD-ORDER-WORKFLOW") ~ "INITIAL",
    Vector(
      ForkList(
        expr("$myList"),
        exprFunction("(element) => $element"),
        exprFunction("(element) => { element: $element }"),
        Workflow.of(
          EmptyJob.execute(agentPath),
          Fail(Some(expr("'TEST FAILURE'")))))))

  private val joinFailingChildOrdersWorkflow = Workflow(
    WorkflowPath("JOIN-FAILING-CHILD-ORDER-WORKFLOW") ~ "INITIAL",
    Vector(
      ForkList(
        expr("$myList"),
        exprFunction("(element) => $element"),
        exprFunction("(element) => { element: $element }"),
        Workflow.of(
          EmptyJob.execute(agentPath),
          Fail(Some(expr("'TEST FAILURE'")))),
        joinIfFailed = true)))

  private val indexWorkflow = Workflow(
    WorkflowPath("INDEX-WORKFLOW") ~ "INITIAL",
    Vector(
      ForkList(
        expr("$myList"),
        exprFunction("(element, i) => $i"),
        exprFunction("(element, i) => { element: $element, index: $i }"),
        Workflow.of(
          If(expr("$index < 0 || $index > 9999"),
            Workflow.of(Fail())),
          EmptyJob.execute(agentPath)))))

  private val exampleWorkflow = Workflow(
    WorkflowPath("EXAMPLE") ~ "INITIAL",
    Vector(
      ForkList(
          expr("$myList"),
          childToId = exprFunction("(element) => $element.id"),
          childToArguments = exprFunction("(element) => { elementId: $element.id }"),
          Workflow.of(
            Execute(WorkflowJob(
              agentPath,
              ShellScriptExecutable(
                if (isWindows)
                  """@echo off
                    |echo ELEMENT_ID=%ELEMENT_ID%
                    |""".stripMargin
                else
                 """#!/usr/bin/env bash
                   |set -euo pipefail
                   |echo ELEMENT_ID=$ELEMENT_ID
                   |""".stripMargin,
                  env = Map(
                    "ELEMENT_ID" -> expr("$elementId")))))))))

  private def newOrder(orderId: OrderId, workflowPath: WorkflowPath, n: Int,
    deleteWhenTerminated: Boolean = false) =
    FreshOrder(
      orderId,
      workflowPath,
      Map("myList" -> ListValue(for (i <- 1 to n) yield StringValue(s"ELEMENT-$i"))),
      deleteWhenTerminated = deleteWhenTerminated)
}
