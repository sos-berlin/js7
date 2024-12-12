package js7.tests.subagent

import cats.effect.unsafe.IORuntime
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem}
import js7.data.value.StringValue
import js7.data.value.expression.Expression.NumericConstant
import js7.data.value.expression.ExpressionParser.{expr, exprFunction}
import js7.data.workflow.instructions.ForkList
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.subagent.SubagentForkListTest.*
import js7.tests.subagent.SubagentTester.agentPath
import js7.tests.testenv.BlockingItemUpdater
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId

final class SubagentForkListTest extends OurTestSuite, SubagentTester, BlockingItemUpdater:

  override protected def agentConfig = config"""
    js7.auth.subagents.BARE-SUBAGENT = "$localSubagentId's PASSWORD"
    js7.auth.subagents.B-SUBAGENT = "$localSubagentId's PASSWORD"
  """.withFallback(super.agentConfig)

  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(
    bareSubagentItem,
    bSubagentItem,
    SubagentBundle(subagentBundleId, Map(
      localSubagentId -> NumericConstant(1),
      bareSubagentId -> NumericConstant(2))))

  private val bSubagentId = SubagentId("B-SUBAGENT")
  private lazy val bSubagentItem = SubagentItem(
    bSubagentId,
    agentPath,
    findFreeLocalUri())

  lazy val (bareSubagent, bareSubagentRelease) = subagentResource(bareSubagentItem)
    .allocated.await(99.s)

  lazy val (bSubagent, bSubagentRelease) = subagentResource(bSubagentItem)
    .allocated.await(99.s)

  override def beforeAll() =
    super.beforeAll()
    bareSubagent
    bSubagent

  override def afterAll() =
    try
      bSubagentRelease.await(99.s)
      bareSubagentRelease.await(99.s)
    finally
      super.afterAll()

  "ForkList with subagentIds function at Controller is rejected" in:
    withItem(Workflow(
      WorkflowPath("FORK-LIST-AT-CONTROLLER"),
      Seq(
        ForkList(
          expr("subagentIds($arg)"),
          exprFunction("(listElement) => $listElement"),
          exprFunction("(listElement) => { subagentId: $listElement }"),
          Workflow.empty)))
    ) { workflow =>
      val orderId = OrderId("CONTROLLER")
      val freshOrder = FreshOrder(orderId, workflow.path, Map(
        "arg" -> StringValue("CONTROLLER")))
      val events = controller.runOrder(freshOrder)
      assert(events.map(_.value) == Seq(
        OrderAdded(workflow.id, Map("arg" -> StringValue("CONTROLLER"))),
        OrderOutcomeAdded(OrderOutcome.Disrupted(Problem(
          "The subagentIds function is available only for ForkList statement running at an Agent" +
            " — use the agentPath argument!"))),
        OrderFailed(Position(0))
      ))
    }

  "ForkList fails due to unknown SubagentBundleId" in:
    val workflow = Workflow(
      WorkflowPath("UNKNOWN-SUBAGENT-BUNDLE-ID"),
      Seq(
        ForkList(
          expr("subagentIds($arg)"),
          exprFunction("(listElement) => $listElement"),
          exprFunction("(listElement) => { subagentId: $listElement }"),
          Workflow.of(
            EmptyJob.execute(
              agentPath,
              subagentBundleId = Some(expr("$subagentId")),
              processLimit = 100)),
          agentPath = Some(agentPath))))
    withItem(workflow) { workflow =>
      val orderId = OrderId("UNKNOWN")
      val freshOrder = FreshOrder(orderId, workflow.path, Map(
        "arg" -> StringValue("UNKNOWN")))
      val events = controller.runOrder(freshOrder)
      assert(events.map(_.value) == Seq(
        OrderAdded(workflow.id, Map("arg" -> StringValue("UNKNOWN"))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderOutcomeAdded(OrderOutcome.Disrupted(UnknownKeyProblem("SubagentBundleId", "SubagentBundle:UNKNOWN"))),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(0))
      ))
    }

  "ForkList over the SubagentIds of SubagentBundleIds" in:
    val workflow = Workflow(
      WorkflowPath("UNKNOWN-SUBAGENT-BUNDLE-ID"),
      Seq(
        ForkList(
          expr("subagentIds($arg)"),
          exprFunction("(listElement) => $listElement"),
          exprFunction("(listElement) => { subagentId: $listElement }"),
          Workflow.of(
            EmptyJob.execute(
              agentPath,
              subagentBundleId = Some(expr("$subagentId")),
              processLimit = 100)),
          agentPath = Some(agentPath))))

    withItem(workflow) { workflow =>
      val orderId = OrderId("SUBAGENT-BUNDLE")
      val localSubagentOrderId = orderId / localSubagentId.string
      val bareSubagentOrderId = orderId / bareSubagentId.string
      val events = controller.runOrder(FreshOrder(orderId, workflow.path, Map(
        "arg" -> StringValue(subagentBundleId.string))))
      assert(events.map(_.value) == Seq(
        OrderAdded(workflow.id, Map(
          "arg" -> StringValue(subagentBundleId.string))),
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
        OrderJoined(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderFinished()))

      assert(eventWatch.eventsByKey[OrderEvent](localSubagentOrderId) == Seq(
        OrderProcessingStarted(localSubagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(0) / "fork" % 1),
        OrderDetachable,
        OrderDetached))

      assert(eventWatch.eventsByKey[OrderEvent](bareSubagentOrderId) == Seq(
        OrderProcessingStarted(bareSubagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(0) / "fork" % 1),
        OrderDetachable,
        OrderDetached))
    }

  "ForkList over the SubagentIds of the Agent" in:
    val workflow = Workflow(
      WorkflowPath("UNKNOWN-SUBAGENT-BUNDLE-ID"),
      Seq(
        ForkList(
          expr("subagentIds()"),
          exprFunction("(listElement) => $listElement"),
          exprFunction("(listElement) => { subagentId: $listElement }"),
          Workflow.of(
            EmptyJob.execute(
              agentPath,
              subagentBundleId = Some(expr("$subagentId")),
              processLimit = 100)),
          agentPath = Some(agentPath))))

    withItem(workflow) { workflow =>
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
        OrderJoined(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderFinished()))

      assert(eventWatch.eventsByKey[OrderEvent](localSubagentOrderId) == Seq(
        OrderProcessingStarted(localSubagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(0) / "fork" % 1),
        OrderDetachable,
        OrderDetached))

      assert(eventWatch.eventsByKey[OrderEvent](bareSubagentOrderId) == Seq(
        OrderProcessingStarted(bareSubagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(0) / "fork" % 1),
        OrderDetachable,
        OrderDetached))
      assert(eventWatch.eventsByKey[OrderEvent](bSubagentOrderId) == Seq(
        OrderProcessingStarted(bSubagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(0) / "fork" % 1),
        OrderDetachable,
        OrderDetached))
    }


object SubagentForkListTest:
  private val localSubagentId = toLocalSubagentId(agentPath)
  private val subagentBundleId = SubagentBundleId("SUBAGENT-BUNDLE")
