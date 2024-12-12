package js7.tests.notice

import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.{BoardPath, BoardState, GlobalBoard, NoticeId}
import js7.data.controller.ControllerCommand.{PostNotice, TransferOrders}
import js7.data.order.OrderEvent.{OrderNoticesExpected, OrderTerminated, OrderTransferred}
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.workflow.instructions.{ConsumeNotices, ExpectNotices}
import js7.data.workflow.{Instruction, Workflow, WorkflowId, WorkflowPath}
import js7.tests.testenv.ControllerAgentForScalaTest
import org.scalatest.Assertion
import scala.concurrent.duration.*

trait TransferOrdersWaitingForNoticeTest:
  this: ControllerAgentForScalaTest =>

  protected final def testTransferOrders(
    toNoticeInstr: BoardPath => ExpectNotices | ConsumeNotices,
    assertResult: (GlobalBoard, GlobalBoard, WorkflowId, WorkflowId, OrderId, NoticeId) => Assertion)
  : Unit =
    var eventId = eventWatch.lastAddedEventId
    val qualifier = "2024-08-23"
    val orderId = OrderId(s"#$qualifier#TRANSFER")
    val noticeId = NoticeId(qualifier)

    val board1 = GlobalBoard.joc(BoardPath("TRANSFER-1"), Some(1.day))
    val board2 = GlobalBoard.joc(BoardPath("TRANSFER-2"), Some(1.day))
    val workflow = Workflow(WorkflowPath("TRANSFER"), Seq(
      toNoticeInstr(board1.path)))

    withItems((board1, board2, workflow)): (board1, board2, workflow) =>
      assert(controllerState.keyTo(BoardState)(board1.path) ==
        BoardState(board1, idToNotice = Map.empty, orderToConsumptionStack = Map.empty))

      controller.api.addOrder:
        FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
      .await(99.s).orThrow
      eventWatch.await[OrderNoticesExpected](_.key == orderId, after = eventId)

      /// newWorkflow with changed ExpectNotices/ConsumeNotices instruction ///
      val newWorkflow = Workflow(workflow.path, Seq(toNoticeInstr(board2.path)))
      withItem(newWorkflow): newWorkflow =>
        eventId = eventWatch.lastWatchedEventId

        /// TransferOrders ///
        execCmd(TransferOrders(workflow.id))
        eventWatch.await[OrderTransferred](_.key == orderId, after = eventId)

        // Let order in newWorkflow continue
        controller.api.executeCommand(PostNotice(board2.path, noticeId)).await(99.s).orThrow
        eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)

        assertResult(board1, board2, workflow.id, newWorkflow.id, orderId, noticeId)
