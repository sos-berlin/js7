package js7.tests

import cats.effect.unsafe.IORuntime
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.DurationRichInt
import js7.data.agent.AgentPath
import js7.data.board.BoardPathExpression.ExpectNotice
import js7.data.board.{BoardPath, BoardPathExpression, GlobalBoard, NoticeKey}
import js7.data.command.SuspensionMode
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, PostNotice, ResumeOrder, SuspendOrders, TransferOrders}
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.order.OrderEvent.OrderNoticesConsumptionStarted.Consumption
import js7.data.order.OrderEvent.OrderNoticesExpected.Expected
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderProcessed, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderStarted, OrderStateReset, OrderStdoutWritten, OrderSuspended, OrderSuspensionMarked, OrderSuspensionMarkedOnAgent, OrderTerminated, OrderTransferred}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.plan.PlanId
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{ConsumeNotices, Fork, Prompt}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{InstructionNr, Label, Position}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.TransferOrderTest.*
import js7.tests.jobs.{EmptyJob, SemaphoreJob}
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId

final class TransferOrderTest
extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.order.stdout-stderr.delay = 1ms"""

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(board)

  "TransferOrder" in:
    val workflow = Workflow.of(WorkflowPath("WORKFLOW"),
      "A" @: Prompt(expr("'?'")),
      "B" @: BSemaphoreJob.execute(agentPath),
      "C" @: Fork.of("BRANCH" -> Workflow.of(
        CSemaphoreJob.execute(agentPath))),
      "D" @: ConsumeNotices(ExpectNotice(board.path)):
          EmptyJob.execute(agentPath))

    withItem(workflow): workflow1 =>
      /// aOrderId sticks in Order.Prompting, is transferable ///
      val aOrderId = OrderId("A-ORDER")
      controller.api
        .addOrder(FreshOrder(aOrderId, workflow1.path, stopPositions = Set(Position(1))))
        .await(99.s).orThrow
      eventWatch.await[OrderPrompted](_.key == aOrderId)

      /// bOrderId sticks in Order.Processing, is not transferable because Attached ///
      val bOrderId = OrderId("B-ORDER")
      controller.api
        .addOrder(FreshOrder(bOrderId, workflow1.path,
          startPosition = Some(Label("B")),
          stopPositions = Set(Label("C"))))
        .await(99.s).orThrow
      eventWatch.await[OrderStdoutWritten](_.key == bOrderId)

      /// cOrderId is forked and attached to agentPath, too ///
      val cOrderId = OrderId("C-ORDER")
      controller.api
        .addOrder(FreshOrder(cOrderId, workflow1.path,
          startPosition = Some(Label("C")),
          stopPositions = Set(Label("D"))))
        .await(99.s).orThrow
      val cChildOrderId = cOrderId / "BRANCH"
      eventWatch.await[OrderStdoutWritten](_.key == cChildOrderId)

      /// dOrderId is Order.ConsumeNotices ///
      val dOrderId = OrderId("D-ORDER")
      controller.api
        .addOrder(FreshOrder(dOrderId, workflow1.path,
          startPosition = Some(Label("D"))))
        .await(99.s).orThrow
      eventWatch.await[OrderNoticesExpected](_.key == dOrderId)

      /// TransferOrders will fail ///
      val workflow2 = updateItem(workflow)
      val checked = controller.api.executeCommand(TransferOrders(workflow1.id)).await(99.s)
      assert(checked == Left(Problem(
        "Order:B-ORDER is required to be detached, but it is Attached to Agent:AGENT;\n" +
        "Order:C-ORDER|BRANCH is required to be detached, but it is Attached to Agent:AGENT")))

      /// Detach bOrderId ///
      execCmd(SuspendOrders(Set(bOrderId)))
      eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == bOrderId)
      BSemaphoreJob.continue()
      eventWatch.await[OrderSuspended](_.key == bOrderId)
      assert(controllerState.idToOrder(bOrderId).isDetached)

      /// Detach cChildOrderId ///
      execCmd(SuspendOrders(Set(cChildOrderId)))
      eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == cChildOrderId)
      CSemaphoreJob.continue()
      eventWatch.await[OrderSuspended](_.key == cChildOrderId)
      assert(controllerState.idToOrder(cChildOrderId).isDetached)

      /// TransferOrders succeeds ///
      execCmd(TransferOrders(workflow1.id))

      /// All orders have been transferred ///
      for orderId <- Seq(aOrderId, bOrderId, cOrderId, cChildOrderId, dOrderId) do
        withClue(s"$orderId: "):
          assert(controllerState.idToOrder(orderId).workflowId == workflow2.id)

      assert(eventWatch.eventsByKey[OrderEvent](aOrderId) == Seq(
        OrderAdded(workflow1.id, stopPositions = Set(Position(1))),
        OrderStarted,
        OrderPrompted(StringValue("?")),
        OrderTransferred(workflow2.id /: Position(0))))

      assert(eventWatch.eventsByKey[OrderEvent](bOrderId) == Seq(
        OrderAdded(workflow1.id,
          startPosition = Some(Position(1)), stopPositions = Set(Label("C"))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("BSemaphoreJob\n"),
        OrderSuspensionMarked(SuspensionMode()),
        OrderSuspensionMarkedOnAgent,
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(2)),
        OrderDetachable,
        OrderDetached,
        OrderSuspended,
        OrderTransferred(workflow2.id /: Position(2))))

      eventWatch.await[ItemDeleted](_.event.key == workflow1.id)

      execCmd(AnswerOrderPrompt(aOrderId))
      eventWatch.await[OrderFinished](_.key == aOrderId)

      execCmd(ResumeOrder(bOrderId))
      eventWatch.await[OrderFinished](_.key == bOrderId)

      // Forked cOrderId finished, too
      execCmd(ResumeOrder(cChildOrderId))
      eventWatch.await[OrderFinished](_.key == cOrderId)

      // Notice-consuming dOrderId finished, too
      execCmd(PostNotice(PlanId.Global / board.path / NoticeKey("NOTICE")))
      eventWatch.await[OrderFinished](_.key == dOrderId)

  "TransferOrder with changed ConsumeNotices instruction in surrounding block is rejected" in:
    BSemaphoreJob.reset()

    val aBoard = GlobalBoard.joc(BoardPath("BOARD-A"))
    val bBoard = GlobalBoard.joc(BoardPath("BOARD-B"))
    val workflow = Workflow.of(WorkflowPath("WORKFLOW"),
      ConsumeNotices(ExpectNotice(aBoard.path)):
        Prompt(expr("'PROMPT-1'")))

    withItems((aBoard, bBoard, workflow)): (aBoard, bBoard, workflow1) =>
      val eventId = eventWatch.lastAddedEventId
      val qualifier = "2024-08-23"
      val noticeId = PlanId.Global / aBoard.path / NoticeKey(qualifier)
      val orderId = OrderId(s"#$qualifier#")

      controller.api
        .addOrder(FreshOrder(orderId, workflow1.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      execCmd(PostNotice(noticeId))
      eventWatch.await[OrderPrompted](_.key == orderId, after = eventId)

      /// TransferOrders with changed ConsumeNotices instructions will fail ///
      updateItem:
        Workflow.of(workflow1.path,
          ConsumeNotices(ExpectNotice(bBoard.path)):
            Prompt(expr("'PROMPT-2a'")))

      val checked = controller.api.executeCommand(TransferOrders(workflow1.id)).await(99.s)
      assert(checked == Left(Problem:
        "Order:#2024-08-23# is not transferable because the surrounding ConsumeNotices instruction at position 0 has changed"))

      /// TransferOrders with unchanged ConsumeNotices instructions will succeed ///
      val workflow3 = updateItem:
        Workflow.of(workflow1.path,
          ConsumeNotices(ExpectNotice(aBoard.path)):
            Prompt(expr("'PROMPT-2b'"))) // But block of ConsumeNotices changed!

      execCmd(TransferOrders(workflow1.id))

      execCmd(AnswerOrderPrompt(orderId))
      eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)

      eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow1.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderNoticesExpected(Vector(
          Expected(aBoard.path, noticeId.noticeKey))),
        OrderNoticesConsumptionStarted(Vector(
          Consumption(aBoard.path, noticeId.noticeKey))),
        OrderPrompted(StringValue("PROMPT-1")),
        OrderStateReset,
        OrderTransferred(workflow3.id /: (Position(0) / "consumeNotices" % 0)),
        OrderPromptAnswered(),
        OrderMoved(Position(0) / "consumeNotices" % 1),
        OrderNoticesConsumed(),
        OrderFinished(),
        OrderDeleted)


object TransferOrderTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  final class BSemaphoreJob extends SemaphoreJob(BSemaphoreJob)
  object BSemaphoreJob extends SemaphoreJob.Companion[BSemaphoreJob]

  final class CSemaphoreJob extends SemaphoreJob(CSemaphoreJob)
  object CSemaphoreJob extends SemaphoreJob.Companion[CSemaphoreJob]

  final class DSemaphoreJob extends SemaphoreJob(DSemaphoreJob)
  object DSemaphoreJob extends SemaphoreJob.Companion[DSemaphoreJob]

  private val board = GlobalBoard.singleNotice(BoardPath("BOARD"))
