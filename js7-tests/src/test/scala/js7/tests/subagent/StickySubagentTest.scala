package js7.tests.subagent

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.AnswerOrderPrompt
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderStarted, OrderStickySubagentEntered, OrderStickySubagentLeaved, OrderTerminated}
import js7.data.order.Outcome.Disrupted.ProcessLost
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, Outcome}
import js7.data.subagent.Problems.{ProcessLostDueToRestartProblem, SubagentNotDedicatedProblem}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentCouplingFailed}
import js7.data.subagent.{SubagentId, SubagentItem, SubagentSelection, SubagentSelectionId}
import js7.data.value.StringValue
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{Fail, Fork, Prompt, StickySubagent}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.subagent.StickySubagentTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler

final class StickySubagentTest extends OurTestSuite with ControllerAgentForScalaTest
{
  // See also SubagentKeeperTest, determineSubagentSelection

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(aAgentPath, bAgentPath)

  override protected lazy val extraSubagentItems = Seq(
    SubagentItem(a1SubagentId, aAgentPath, findFreeLocalUri()),
    SubagentItem(a2SubagentId, aAgentPath, findFreeLocalUri()))

  protected lazy val items = Seq(
    withSubagentSelectionWorkflow, withoutSubagentSelectionWorkflow,
    simpleWithSubagentSelectionWorkflow,
    failAtControllerWorkflow, failAtAgentWorkflow,
    aSubagentSelection, a2SubagentSelection)

  protected implicit val scheduler = Scheduler.traced

  "StickySubagent with SubagentSelection" in {
    enableSubagents(a1SubagentId -> false, a2SubagentId -> false)

    val orderId = OrderId("♣️")
    controller.addOrderBlocking(FreshOrder(orderId, withSubagentSelectionWorkflow.path))

    eventWatch.await[OrderPrompted](_.key == orderId)
    assert(controllerState.idToOrder(orderId).stickySubagents == List(
      Order.StickySubagent(
        aAgentPath,
        Some(aSubagentSelection.id),
        stuckSubagentId = Some(aSubagentId))))
    controller.api.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow

    enableSubagents(aSubagentId -> true, a1SubagentId -> true, a2SubagentId -> true)

    eventWatch.await[OrderTerminated](_.key == orderId)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(withSubagentSelectionWorkflow.id),
      OrderStickySubagentEntered(aAgentPath, Some(aSubagentSelection.id)),
      OrderAttachable(aAgentPath),
      OrderAttached(aAgentPath),
      OrderStarted,

      OrderProcessingStarted(Some(a2SubagentId)),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 1),

      OrderProcessingStarted(Some(aSubagentId), stick = true),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 2),
      OrderDetachable,
      OrderDetached,

      OrderPrompted(StringValue("PROMPT")),
      OrderPromptAnswered(),
      OrderMoved(Position(0) / "stickySubagent" % 3),

      OrderAttachable(aAgentPath),
      OrderAttached(aAgentPath),
      OrderProcessingStarted(Some(aSubagentId)),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 4),

      OrderProcessingStarted(Some(a2SubagentId)),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 5),
      OrderDetachable,
      OrderDetached,

      OrderAttachable(bAgentPath),
      OrderAttached(bAgentPath),
      OrderProcessingStarted(Some(bSubagentId)),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 6),
      OrderDetachable,
      OrderDetached,

      OrderAttachable(aAgentPath),
      OrderAttached(aAgentPath),
      OrderProcessingStarted(Some(a2SubagentId)),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 7),

      OrderProcessingStarted(Some(aSubagentId)),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 8),

      OrderStickySubagentLeaved,

      OrderDetachable,
      OrderDetached,
      OrderFinished()))
  }

  "StickySubagent without SubagentSelection, but with a forking order" in {
    enableSubagents(aSubagentId -> false, a1SubagentId -> true, a2SubagentId -> false)

    val orderId = OrderId("🔷")
    controller.addOrderBlocking(FreshOrder(orderId, withoutSubagentSelectionWorkflow.path))

    val childOrderId = orderId / "BRANCH"
    eventWatch.await[OrderPrompted](_.key == childOrderId)

    assert(controllerState.idToOrder(orderId).stickySubagents == List(
      Order.StickySubagent(
        aAgentPath,
        None,
        stuckSubagentId = Some(a1SubagentId))))

    assert(controllerState.idToOrder(childOrderId).stickySubagents == List(
      Order.StickySubagent(
        aAgentPath,
        None,
        stuckSubagentId = Some(a1SubagentId))))

    // Stop the stuck a1Subagent to prove that the Order is stuck to it
    enableSubagents(aSubagentId -> true, a1SubagentId -> true, a2SubagentId -> true)
    val eventId = eventWatch.lastAddedEventId
    stopBareSubagent(a1SubagentId)

    controller.api.executeCommand(AnswerOrderPrompt(childOrderId)).await(99.s).orThrow
    eventWatch.await[SubagentCouplingFailed](_.key == a1SubagentId, after = eventId)

    val a1SubagentRelease = startBareSubagent(a1SubagentId)._2
    eventWatch.await[SubagentCoupled](_.key == a1SubagentId, after = eventId)
    eventWatch.await[OrderTerminated](_.key == orderId)
    a1SubagentRelease.await(99.s)

    assert(eventWatch.eventsByKey[OrderEvent](orderId)
      .map {
        case OrderProcessed(Outcome.Disrupted(ProcessLost(ProcessLostDueToRestartProblem))) =>
          OrderProcessed(Outcome.processLost(SubagentNotDedicatedProblem))
        case o => o
      } == Seq(
      OrderAdded(withoutSubagentSelectionWorkflow.id),
      OrderStickySubagentEntered(aAgentPath),
      OrderAttachable(aAgentPath),
      OrderAttached(aAgentPath),
      OrderStarted,

      OrderProcessingStarted(Some(a1SubagentId), stick = true),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 1),

      OrderProcessingStarted(Some(a1SubagentId)),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 2),

      OrderDetachable,
      OrderDetached,
      OrderForked(Vector("BRANCH" -> childOrderId)),
      OrderJoined(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 3),

      OrderStickySubagentLeaved,
      OrderFinished()))

    assert(eventWatch.eventsByKey[OrderEvent](childOrderId) == Seq(
      OrderPrompted(StringValue("PROMPT")),
      OrderPromptAnswered(),
      OrderMoved(Position(0) / "stickySubagent" % 2 / "fork+BRANCH" % 1)))
  }

  "Extra test: StickySubagent with job SubagentSelection" in {
    enableSubagents(a1SubagentId -> false, a2SubagentId -> false)

    val orderId = OrderId("🟦")
    controller.addOrderBlocking(FreshOrder(orderId, simpleWithSubagentSelectionWorkflow.path))

    eventWatch.await[OrderPrompted](_.key == orderId)
    assert(controllerState.idToOrder(orderId).stickySubagents == List(
      Order.StickySubagent(
        aAgentPath,
        Some(aSubagentSelection.id),
        stuckSubagentId = Some(aSubagentId))))
    controller.api.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow

    enableSubagents(aSubagentId -> true, a1SubagentId -> true, a2SubagentId -> true)

    eventWatch.await[OrderTerminated](_.key == orderId)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(simpleWithSubagentSelectionWorkflow.id),

      OrderStickySubagentEntered(aAgentPath, Some(aSubagentSelection.id)),
      OrderAttachable(aAgentPath),
      OrderAttached(aAgentPath),
      OrderStarted,

      OrderProcessingStarted(Some(aSubagentId), stick = true),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 1),
      OrderDetachable,
      OrderDetached,

      OrderPrompted(StringValue("PROMPT")),
      OrderPromptAnswered(),
      OrderMoved(Position(0) / "stickySubagent" % 2),

      OrderAttachable(aAgentPath),
      OrderAttached(aAgentPath),
      OrderProcessingStarted(Some(aSubagentId)),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 3),

      OrderProcessingStarted(Some(aSubagentId)),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 4),

      OrderStickySubagentLeaved,
      OrderDetachable,
      OrderDetached,
      OrderFinished()))
  }

  "Fail in StickySubagent at Controller" in {
    enableSubagents(aSubagentId -> true, a1SubagentId -> false, a2SubagentId -> false)
    val orderId = OrderId("♦️")
    val events = controller.runOrder(FreshOrder(orderId, failAtControllerWorkflow.path))
      .map(_.value)
    assert(events == Seq(
      OrderAdded(failAtControllerWorkflow.id),
      OrderStickySubagentEntered(aAgentPath),
      OrderStarted,
      OrderOutcomeAdded(Outcome.failed),
      OrderStickySubagentLeaved,
      OrderFailed(Position(0))))
  }

  "Fail in StickySubagent at Agent" in {
    enableSubagents(aSubagentId -> true, a1SubagentId -> false, a2SubagentId -> false)
    val orderId = OrderId("♦️♦️")
    val events = controller.runOrder(FreshOrder(orderId, failAtAgentWorkflow.path))
      .map(_.value)
    assert(events == Seq(
      OrderAdded(failAtAgentWorkflow.id),
      OrderStickySubagentEntered(aAgentPath),
      OrderAttachable(aAgentPath),
      OrderAttached(aAgentPath),
      OrderStarted,

      OrderProcessingStarted(Some(aSubagentId), stick = true),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "stickySubagent" % 1),

      OrderOutcomeAdded(Outcome.failed),
      OrderStickySubagentLeaved,
      OrderDetachable,
      OrderDetached,
      OrderFailed(Position(1))))
  }
}

object StickySubagentTest
{
  private val aAgentPath = AgentPath("A-AGENT")
  private val aSubagentId = toLocalSubagentId(aAgentPath)
  private val a1SubagentId = SubagentId("A-1-SUBAGENT")
  private val a2SubagentId = SubagentId("A-2-SUBAGENT")

  private val aSubagentSelection = SubagentSelection(
    SubagentSelectionId("A-SUBAGENT-SELECTION"),
    Map(
      aSubagentId -> 2,
      a1SubagentId -> 1))

  private val a2SubagentSelection = SubagentSelection(
    SubagentSelectionId("A2-SUBAGENT-SELECTION"),
    Map(
      a2SubagentId -> 1))

  private val bAgentPath = AgentPath("B-AGENT")
  private val bSubagentId = toLocalSubagentId(bAgentPath)

  private val withSubagentSelectionWorkflow = Workflow(
    WorkflowPath("STICKY-WITH-SUBAGENT-SELECTION") ~ "INITIAL",
    Seq(
      StickySubagent(
        aAgentPath,
        Some(StringConstant(aSubagentSelection.id.string)),
        subworkflow = Workflow.of(
          EmptyJob.execute(aAgentPath,
            subagentSelectionId = Some(StringConstant(a2SubagentId.string))),
          EmptyJob.execute(aAgentPath),
          Prompt(expr("'PROMPT'")),
          EmptyJob.execute(aAgentPath),
          EmptyJob.execute(aAgentPath,
            subagentSelectionId = Some(StringConstant(a2SubagentSelection.id.string))),
          EmptyJob.execute(bAgentPath),
          EmptyJob.execute(aAgentPath,
            subagentSelectionId = Some(StringConstant(a2SubagentSelection.id.string))),
          EmptyJob.execute(aAgentPath)))))

  private val simpleWithSubagentSelectionWorkflow = Workflow(
    WorkflowPath("SIMPLE-STICKY-WITH-SUBAGENT-SELECTION") ~ "INITIAL",
    Seq(
      StickySubagent(
        aAgentPath,
        Some(StringConstant(aSubagentSelection.id.string)),
        subworkflow = {
          val execute = EmptyJob.execute(aAgentPath,
            subagentSelectionId = Some(StringConstant(aSubagentSelection.id.string)))
          Workflow.of(
            execute,
            Prompt(expr("'PROMPT'")),
            execute,
            execute)
        })))

  private val withoutSubagentSelectionWorkflow = Workflow(
    WorkflowPath("STICKY-WITHOUT-SUBAGENT-SELECTION") ~ "INITIAL",
    Seq(
      StickySubagent(
        aAgentPath,
        subworkflow = Workflow.of(
          EmptyJob.execute(aAgentPath),
          EmptyJob.execute(aAgentPath),
          Fork(
            Vector(
              Fork.Branch(
                "BRANCH",
                Workflow.of(
                  Prompt(expr("'PROMPT'"))))))))))

  private val failAtControllerWorkflow = Workflow(
    WorkflowPath("FAILING-AT-CONTROLLER") ~ "INITIAL",
    Seq(
      StickySubagent(aAgentPath, None,
        subworkflow = Workflow.of(
          Fail()))))

  private val failAtAgentWorkflow = Workflow(
    WorkflowPath("FAILING-IN-AGENT") ~ "INITIAL",
    Seq(
      StickySubagent(aAgentPath, None,
        subworkflow = Workflow.of(
          EmptyJob.execute(aAgentPath),
          Fail()))))
}
