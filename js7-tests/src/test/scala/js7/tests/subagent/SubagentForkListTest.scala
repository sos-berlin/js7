package js7.tests.subagent

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.subagent.{SubagentId, SubagentItem, SubagentSelection, SubagentSelectionId}
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.{expr, exprFunction}
import js7.data.workflow.instructions.ForkList
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.subagent.SubagentForkListTest.*
import js7.tests.testenv.BlockingItemUpdater
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler

final class SubagentForkListTest
extends OurTestSuite with SubagentTester with BlockingItemUpdater
{
  override protected def agentConfig = config"""
    js7.auth.subagents.B-SUBAGENT = "AGENT-PASSWORD"
  """.withFallback(super.agentConfig)

  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(
    bareSubagentItem,
    bSubagentItem,
    SubagentSelection(subagentSelectionId, Map(
      localSubagentId -> 1,
      bareSubagentId -> 2)))

  private val bSubagentId = SubagentId("B-SUBAGENT")
  private lazy val bSubagentItem = SubagentItem(
    bSubagentId,
    agentPath,
    findFreeLocalUri())

  protected implicit val scheduler = Scheduler.traced

  lazy val (bareSubagent, bareSubagentRelease) = subagentResource(bareSubagentItem)
    .allocated.await(99.s)

  lazy val (bSubagent, bSubagentRelease) = subagentResource(bSubagentItem)
    .allocated.await(99.s)

  override def beforeAll() = {
    super.beforeAll()
    bareSubagent
    bSubagent
  }

  override def afterAll() = {
    bSubagentRelease.await(99.s)
    bareSubagentRelease.await(99.s)
    super.afterAll()
  }

  "ForkList with subagentIds function at Controller is rejected" in {
    withTemporaryItem(Workflow(
      WorkflowPath("FORK-LIST-AT-CONTROLLER"),
      Seq(
        ForkList(
          expr("subagentIds($arg)"),
          exprFunction("(listElement) => $listElement"),
          exprFunction("(listElement) => { subagentId: $listElement }"),
          Workflow.empty))))
    { workflow =>
      val orderId = OrderId("CONTROLLER")
      val freshOrder = FreshOrder(orderId, workflow.path, Map(
        "arg" -> StringValue("CONTROLLER")))
      val events = controller.runOrder(freshOrder)
      assert(events.map(_.value) == Seq(
        OrderAdded(workflow.id, Map("arg" -> StringValue("CONTROLLER"))),
        OrderOutcomeAdded(Outcome.Disrupted(Problem(
          "The subagentIds function is available only for ForkList statement running at an Agent" +
            " — use the agentPath argument!"))),
        OrderFailed(Position(0))
      ))
    }
  }

  "ForkList fails due to unknown SubagentSelectionId" in {
    val workflow = Workflow(
      WorkflowPath("UNKNOWN-SUBAGENT-SELECTION-ID"),
      Seq(
        ForkList(
          expr("subagentIds($arg)"),
          exprFunction("(listElement) => $listElement"),
          exprFunction("(listElement) => { subagentId: $listElement }"),
          Workflow.of(
            EmptyJob.execute(
              agentPath,
              subagentSelectionId = Some(expr("$subagentId")),
              parallelism = 100)),
          agentPath = Some(agentPath))))
    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("UNKNOWN")
      val freshOrder = FreshOrder(orderId, workflow.path, Map(
        "arg" -> StringValue("UNKNOWN")))
      val events = controller.runOrder(freshOrder)
      assert(events.map(_.value) == Seq(
        OrderAdded(workflow.id, Map("arg" -> StringValue("UNKNOWN"))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderOutcomeAdded(Outcome.Disrupted(UnknownKeyProblem("SubagentSelectionId", "SubagentSelection:UNKNOWN"))),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(0))
      ))
    }
  }

  "ForkList over the SubagentIds of SubagentSelectionIds" in {
    val workflow = Workflow(
      WorkflowPath("UNKNOWN-SUBAGENT-SELECTION-ID"),
      Seq(
        ForkList(
          expr("subagentIds($arg)"),
          exprFunction("(listElement) => $listElement"),
          exprFunction("(listElement) => { subagentId: $listElement }"),
          Workflow.of(
            EmptyJob.execute(
              agentPath,
              subagentSelectionId = Some(expr("$subagentId")),
              parallelism = 100)),
          agentPath = Some(agentPath))))

    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("SUBAGENT-SELECTION")
      val localSubagentOrderId = orderId / localSubagentId.string
      val bareSubagentOrderId = orderId / bareSubagentId.string
      val events = controller.runOrder(FreshOrder(orderId, workflow.path, Map(
        "arg" -> StringValue(subagentSelectionId.string))))
      assert(events.map(_.value) == Seq(
        OrderAdded(workflow.id, Map(
          "arg" -> StringValue(subagentSelectionId.string))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderForked(Vector(
          OrderForked.Child(localSubagentOrderId, Map(
            "subagentId" -> StringValue("AGENT-0"))),
          OrderForked.Child(bareSubagentOrderId, Map(
            "subagentId" -> StringValue("BARE-SUBAGENT"))))),
        OrderDetachable,
        OrderDetached,
        OrderJoined(Outcome.succeeded),
        OrderMoved(Position(1)),
        OrderFinished()))

      assert(eventWatch.eventsByKey[OrderEvent](localSubagentOrderId) == Seq(
        OrderProcessingStarted(localSubagentId),
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(0) / "fork" % 1),
        OrderDetachable,
        OrderDetached))

      assert(eventWatch.eventsByKey[OrderEvent](bareSubagentOrderId) == Seq(
        OrderProcessingStarted(bareSubagentId),
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(0) / "fork" % 1),
        OrderDetachable,
        OrderDetached))
    }
  }

  "ForkList over the SubagentIds of the Agent" in {
    val workflow = Workflow(
      WorkflowPath("UNKNOWN-SUBAGENT-SELECTION-ID"),
      Seq(
        ForkList(
          expr("subagentIds()"),
          exprFunction("(listElement) => $listElement"),
          exprFunction("(listElement) => { subagentId: $listElement }"),
          Workflow.of(
            EmptyJob.execute(
              agentPath,
              subagentSelectionId = Some(expr("$subagentId")),
              parallelism = 100)),
          agentPath = Some(agentPath))))

    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("ALL-OF-AGENT")
      val localSubagentOrderId = orderId / localSubagentId.string
      val bareSubagentOrderId = orderId / bareSubagentId.string
      val bSubagentOrderId = orderId / bSubagentId.string
      val events = controller.runOrder(FreshOrder(orderId, workflow.path))
      assert(events.map(_.value) == Seq(
        OrderAdded(workflow.id),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderForked(Vector(
          OrderForked.Child(localSubagentOrderId, Map(
            "subagentId" -> StringValue("AGENT-0"))),
          OrderForked.Child(bSubagentOrderId, Map(
            "subagentId" -> StringValue("B-SUBAGENT"))),
          OrderForked.Child(bareSubagentOrderId, Map(
            "subagentId" -> StringValue("BARE-SUBAGENT"))))),
        OrderDetachable,
        OrderDetached,
        OrderJoined(Outcome.succeeded),
        OrderMoved(Position(1)),
        OrderFinished()))

      assert(eventWatch.eventsByKey[OrderEvent](localSubagentOrderId) == Seq(
        OrderProcessingStarted(localSubagentId),
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(0) / "fork" % 1),
        OrderDetachable,
        OrderDetached))

      assert(eventWatch.eventsByKey[OrderEvent](bareSubagentOrderId) == Seq(
        OrderProcessingStarted(bareSubagentId),
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(0) / "fork" % 1),
        OrderDetachable,
        OrderDetached))
      assert(eventWatch.eventsByKey[OrderEvent](bSubagentOrderId) == Seq(
        OrderProcessingStarted(bSubagentId),
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(0) / "fork" % 1),
        OrderDetachable,
        OrderDetached))
    }
  }
}

object SubagentForkListTest
{
  private val agentPath = AgentPath("AGENT")
  private val localSubagentId = toLocalSubagentId(agentPath)
  private val subagentSelectionId = SubagentSelectionId("SUBAGENT-SELECTION")
}
