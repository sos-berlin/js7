package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.DurationRichInt
import js7.data.agent.AgentPath
import js7.data.board.{Board, BoardPath, BoardPathExpression, NoticeId}
import js7.data.command.SuspensionMode
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, PostNotice, ResumeOrder, SuspendOrders, TransferOrders}
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderNoticesExpected, OrderProcessed, OrderProcessingStarted, OrderPrompted, OrderStarted, OrderStdoutWritten, OrderSuspended, OrderSuspensionMarked, OrderSuspensionMarkedOnAgent, OrderTransferred}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{ConsumeNotices, Fork, Prompt}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.TransferOrderTest.*
import js7.tests.jobs.{EmptyJob, SemaphoreJob}
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import monix.execution.Scheduler.Implicits.traced

final class TransferOrderTest
extends OurTestSuite
with ControllerAgentForScalaTest
with BlockingItemUpdater:
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.order.stdout-stderr.delay = 1ms"""

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(noticeBoad)

  "TransferOrder" in:
    withTemporaryItem(workflow) { v1Workflow =>
      // aOrderId sticks in Order.Prompting, is transferable
      val aOrderId = OrderId("A-ORDER")
      controller.api
        .addOrder(FreshOrder(aOrderId, v1Workflow.path, stopPositions = Set(Position(1))))
        .await(99.s).orThrow
      eventWatch.await[OrderPrompted](_.key == aOrderId)

      // bOrderId sticks in Order.Processing, is not transferable because Attached
      val bOrderId = OrderId("B-ORDER")
      controller.api
        .addOrder(FreshOrder(bOrderId, v1Workflow.path,
          startPosition = Some(Position(1)),
          stopPositions = Set(Position(2))))
        .await(99.s).orThrow
      eventWatch.await[OrderStdoutWritten](_.key == bOrderId)

      // cOrderId is forked and attached to agentPath, too
      val cOrderId = OrderId("C-ORDER")
      controller.api
        .addOrder(FreshOrder(cOrderId, v1Workflow.path,
          startPosition = Some(Position(2)), stopPositions = Set(Position(3))))
        .await(99.s).orThrow
      val cChildOrderId = cOrderId / "BRANCH"
      eventWatch.await[OrderStdoutWritten](_.key == cChildOrderId)

      // dOrderId is Order.ConsumeNotices
      val dOrderId = OrderId("D-ORDER")
      controller.api
        .addOrder(FreshOrder(dOrderId, v1Workflow.path, startPosition = Some(Position(3))))
        .await(99.s).orThrow
      eventWatch.await[OrderNoticesExpected](_.key == dOrderId)

      // TransferOrders will fail
      val v2Workflow = updateItem(workflow)
      val checked = controller.api.executeCommand(TransferOrders(v1Workflow.id)).await(99.s)
      assert(checked == Left(Problem(
        "Order:B-ORDER to be transferred is Attached to Agent:AGENT;\n" +
        "Order:C-ORDER|BRANCH to be transferred is Attached to Agent:AGENT")))

      // Detach bOrderId
      controller.api.executeCommand(SuspendOrders(Set(bOrderId))).await(99.s).orThrow
      eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == bOrderId)
      ASemaphoreJob.continue()
      eventWatch.await[OrderSuspended](_.key == bOrderId)
      assert(controllerState.idToOrder(bOrderId).isDetached)

      // Detach cChildOrderId
      controller.api.executeCommand(SuspendOrders(Set(cChildOrderId))).await(99.s).orThrow
      eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == cChildOrderId)
      CSemaphoreJob.continue()
      eventWatch.await[OrderSuspended](_.key == cChildOrderId)
      assert(controllerState.idToOrder(cChildOrderId).isDetached)

      // TransferOrders will succeed, all orders are transferable
      controller.api.executeCommand(TransferOrders(v1Workflow.id)).await(99.s).orThrow

      for orderId <- Seq(aOrderId, bOrderId, cOrderId, cChildOrderId, dOrderId) do
        withClue(s"$orderId: "):
          assert(controllerState.idToOrder(orderId).workflowId == v2Workflow.id)

      assert(eventWatch.eventsByKey[OrderEvent](aOrderId) == Seq(
        OrderAdded(v1Workflow.id, stopPositions = Set(Position(1))),
        OrderStarted,
        OrderPrompted(StringValue("?")),
        OrderTransferred(v2Workflow.id /: Position(0))))

      assert(eventWatch.eventsByKey[OrderEvent](bOrderId) == Seq(
        OrderAdded(v1Workflow.id,
          startPosition = Some(Position(1)), stopPositions = Set(Position(2))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("ASemaphoreJob\n"),
        OrderSuspensionMarked(SuspensionMode(None)),
        OrderSuspensionMarkedOnAgent,
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(2)),
        OrderDetachable,
        OrderDetached,
        OrderSuspended,
        OrderTransferred(v2Workflow.id /: Position(2))))

      eventWatch.await[ItemDeleted](_.event.key == v1Workflow.id)

      controller.api.executeCommand(AnswerOrderPrompt(aOrderId)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == aOrderId)

      controller.api.executeCommand(ResumeOrder(bOrderId)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == bOrderId)

      // Forked cOrderId finished, too
      controller.api.executeCommand(ResumeOrder(cChildOrderId)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == cOrderId)

      // Notice-consuming dOrderId finished, too
      controller.api.executeCommand(PostNotice(noticeBoad.path, NoticeId("NOTICE")))
        .await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == dOrderId)
    }


object TransferOrderTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]

  final class CSemaphoreJob extends SemaphoreJob(CSemaphoreJob)
  object CSemaphoreJob extends SemaphoreJob.Companion[CSemaphoreJob]

  private val noticeBoad = Board.singleNotice(BoardPath("BOARD"))

  private val workflow = Workflow(WorkflowPath("WORKFLOW"),
    Seq(
      Prompt(expr("'?'")),
      ASemaphoreJob.execute(agentPath),
      Fork.of("BRANCH" -> Workflow.of(
        CSemaphoreJob.execute(agentPath))),
      ConsumeNotices(
        BoardPathExpression.ExpectNotice(noticeBoad.path),
        Workflow.of(
          EmptyJob.execute(agentPath)))))
