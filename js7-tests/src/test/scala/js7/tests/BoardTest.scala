package js7.tests

import com.google.inject.{AbstractModule, Provides}
import javax.inject.Singleton
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.time.{AlarmClock, TestAlarmClock, Timestamp}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.Problems.ItemIsStillReferencedProblem
import js7.data.agent.AgentPath
import js7.data.board.BoardEvent.NoticeDeleted
import js7.data.board.{Board, BoardPath, BoardState, Notice, NoticeId}
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{CancelOrders, DeleteNotice, ResumeOrder, SuspendOrders}
import js7.data.item.ItemOperation.{AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.order.Order.Fresh
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderNoticeExpected, OrderNoticeRead, OrderNoticePosted, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderSuspended, OrderSuspensionMarked}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{ExpectNotice, PostNotices}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.BoardTest._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.View
import scala.concurrent.duration._

final class BoardTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 1ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected def agentPaths = Seq(agentPath)

  protected def items = boards ++ Seq(
    posting1Workflow, expecting1Workflow,
    posting3Workflow, expecting3Workflow,
    postingAgentWorkflow, expectingAgentWorkflow)

  private val clock = TestAlarmClock(startTimestamp)

  override protected def controllerModule = new AbstractModule {
    @Provides @Singleton def provideAlarmClock(): AlarmClock = clock
  }

  "Post three notices, then expect them" in {
    val qualifier = nextQualifier()
    val notices = for ((boardPath, endOfLife) <- boards.map(_.path) zip endOfLifes) yield
      Notice(NoticeId(qualifier), boardPath, endOfLife)
    val Seq(notice1, notice2, notice3) = notices

    val posterEvents = controller.runOrder(FreshOrder(OrderId(s"#$qualifier#POSTING"), posting3Workflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(posting3Workflow.id),
      OrderStarted,
      OrderNoticePosted(notice1),
      OrderNoticePosted(notice2),
      OrderNoticePosted(notice3),
      OrderMoved(Position(1)),
      OrderFinished))

    assert(controllerState.pathToBoardState == Map(
      board0.path -> BoardState(
        board0.withRevision(Some(ItemRevision(0))),
        Map(notice1.id -> notice1)),
      board1.path -> BoardState(
        board1.withRevision(Some(ItemRevision(0))),
        Map(notice2.id -> notice2)),
      board2.path -> BoardState(
        board2.withRevision(Some(ItemRevision(0))),
        Map(notice3.id -> notice3))))

    val readerEvents = controller.runOrder(FreshOrder(OrderId(s"#$qualifier#EXPECTING"), expecting3Workflow.path))
    assert(readerEvents.map(_.value) == Seq(
      OrderAdded(expecting3Workflow.id),
      OrderStarted,
      OrderNoticeRead,
      OrderMoved(Position(1)),
      OrderNoticeRead,
      OrderMoved(Position(2)),
      OrderNoticeRead,
      OrderMoved(Position(3)),
      OrderFinished))

    assert(controllerState.pathToBoardState == Map(
      board0.path -> BoardState(
        board0.withRevision(Some(ItemRevision(0))),
        Map(notice1.id -> notice1)),
      board1.path -> BoardState(
        board1.withRevision(Some(ItemRevision(0))),
        Map(notice2.id -> notice2)),
      board2.path -> BoardState(
        board2.withRevision(Some(ItemRevision(0))),
        Map(notice3.id -> notice3))))
  }

  "Two orders expect a notice, then post the notice" in {
    val qualifier = nextQualifier()
    val notice = Notice(NoticeId(qualifier), board0.path, endOfLife0)

    val expectingOrderIds = Seq(OrderId(s"#$qualifier#EXPECTING-A"), OrderId(s"#$qualifier#EXPECTING-B"))
    controllerApi.addOrders(
      Observable.fromIterable(expectingOrderIds).map(FreshOrder(_, expecting1Workflow.path))
    ).await(99.s).orThrow
    for (orderId <- expectingOrderIds) eventWatch.await[OrderNoticeExpected](_.key == orderId)

    val posterEvents = controller.runOrder(
      FreshOrder(OrderId(s"#$qualifier#POSTING"), posting1Workflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(posting1Workflow.id),
      OrderStarted,
      OrderNoticePosted(notice),
      OrderMoved(Position(1)),
      OrderFinished))

    assert(controllerState.pathToBoardState(board0.path) ==
      BoardState(
        board0.withRevision(Some(ItemRevision(0))),
        Map(
          NoticeId("2222-01-01") -> Notice(NoticeId("2222-01-01"), board0.path, endOfLife0),  // from previous test
          notice.id -> notice)))

    for (orderId <- expectingOrderIds) {
      eventWatch.await[OrderFinished](_.key == orderId)
      val expectingEvents = eventWatch.eventsByKey[OrderCoreEvent](orderId)
      assert(expectingEvents == Seq(
        OrderAdded(expecting1Workflow.id),
        OrderStarted,
        OrderNoticeExpected(notice.id),
        OrderNoticeRead,
        OrderMoved(Position(1)),
        OrderFinished))
    }

    assert(controllerState.pathToBoardState(board0.path) ==
      BoardState(
        board0.withRevision(Some(ItemRevision(0))),
        View(
          Notice(NoticeId("2222-01-01"), board0.path, endOfLife0),  // from previous test
          notice
        ).toKeyedMap(_.id)))
  }

  "Detach order when at Agent" in {
    // TODO Post kann am Agenten ausgeführt werden, wenn Board (ohne BoardState) dahin übertragen wird,
    //  und anschließend der Controller Order.ExpectingNotice löst.
    val qualifier = nextQualifier()
    val notice = Notice(NoticeId(qualifier), board0.path, endOfLife0)

    val posterEvents = controller.runOrder(
      FreshOrder(OrderId(s"#$qualifier#POSTING"), postingAgentWorkflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(postingAgentWorkflow.id),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderNoticePosted(notice),
      OrderMoved(Position(2)),
      OrderFinished))

    val readerEvents = controller.runOrder(
      FreshOrder(OrderId(s"#$qualifier#EXPECTING"), expectingAgentWorkflow.path))
    assert(readerEvents.map(_.value) == Seq(
      OrderAdded(expectingAgentWorkflow.id),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderNoticeRead,
      OrderMoved(Position(2)),
      OrderFinished))
  }

  "PostNotices command with expecting orders" in {
    val qualifier = "2222-08-08"
    val noticeId = NoticeId(qualifier)

    val orderIds = Seq(OrderId(s"#$qualifier#EXPECTING-A"), OrderId(s"#$qualifier#EXPECTING-B"))
    controllerApi.addOrders(
      Observable.fromIterable(orderIds).map(FreshOrder(_, expectingAgentWorkflow.path))
    ).await(99.s).orThrow
    for (orderId <- orderIds) eventWatch.await[OrderNoticeExpected](_.key == orderId)
    controllerApi.executeCommand(
      ControllerCommand.PostNotice(board0.path, noticeId)
    ).await(99.s).orThrow
    for (orderId <- orderIds) eventWatch.await[OrderNoticeRead](_.key == orderId)

    val notice2 = Notice(NoticeId("2222-08-09"), board0.path, endOfLife0)
    controllerApi.executeCommand(
      ControllerCommand.PostNotice(board0.path, notice2.id)
    ).await(99.s).orThrow
    sleep(100.ms)
    assert(controllerState.pathToBoardState(board0.path).idToNotice(notice2.id) == notice2)
  }

  "PostNotices command without expecting order" in {
    val notice = Notice(NoticeId("2222-08-09"), board0.path, endOfLife0)
    controllerApi.executeCommand(
      ControllerCommand.PostNotice(board0.path, notice.id)
    ).await(99.s).orThrow

    // With explicit endOfLife
    val notice2 = Notice(NoticeId("2222-08-10"), board0.path, Timestamp("2222-08-09T12:00:00Z"))
    controllerApi.executeCommand(
      ControllerCommand.PostNotice(board0.path, notice2.id,
        endOfLife = Some(notice2.endOfLife))
    ).await(99.s).orThrow

    sleep(100.ms)
    assert(controllerState.pathToBoardState(board0.path).idToNotice(notice.id) == notice)
    assert(controllerState.pathToBoardState(board0.path).idToNotice(notice2.id) == notice2)
  }

  "DeleteNotice command" in {
    val qualifier = "2222-09-09"
    val notice = Notice(NoticeId(qualifier), board0.path, endOfLife0)

    val posterEvents = controller.runOrder(
      FreshOrder(OrderId(s"#$qualifier#POSTING"), posting1Workflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(posting1Workflow.id),
      OrderStarted,
      OrderNoticePosted(notice),
      OrderMoved(Position(1)),
      OrderFinished))

    val eventId = eventWatch.lastAddedEventId

    assert(controllerApi.executeCommand(DeleteNotice(board0.path, NoticeId("UNKNOWN"))).await(99.s) ==
      Left(UnknownKeyProblem("NoticeId", "NoticeId:UNKNOWN")))

    assert(controllerApi.executeCommand(DeleteNotice(BoardPath("UNKNOWN"), notice.id)).await(99.s) ==
      Left(UnknownKeyProblem("BoardPath", "Board:UNKNOWN")))

    controllerApi.executeCommand(DeleteNotice(board0.path, notice.id)).await(99.s).orThrow
    assert(eventWatch.await[NoticeDeleted](_.key == board0.path, after = eventId).head.value.event ==
      NoticeDeleted(notice.id))
  }

  "Delete notice after endOfLife" in {
    clock := endOfLife0 - 1.s
    sleep(100.ms)
    val eventId = eventWatch.lastAddedEventId
    // NoticeDeleted do not occur before endOfLife
    clock := endOfLife0
    // Spare noticeIds.head for DeleteNotice test
    for (noticeId <- noticeIds) {
      eventWatch.await[NoticeDeleted](_.event.noticeId == noticeId, after = eventId)
    }
  }

  "PostNotices and ExpectNotice respect Order.scheduledFor" in {
    val qualifier = "2222-10-10"
    val posterOrderId = OrderId(s"#$qualifier#POSTER")
    val expectingOrderId = OrderId(s"#$qualifier#EXPECTING")
    val startAt = startTimestamp + 10.days

    controllerApi
      .addOrders(Observable(
        FreshOrder(posterOrderId, posting1Workflow.path, scheduledFor = Some(startAt)),
        FreshOrder(expectingOrderId, expecting1Workflow.path, scheduledFor = Some(startAt))))
      .await(99.s).orThrow
    sleep(500.ms)
    val idToOrder = controllerApi.controllerState.await(99.s).orThrow.idToOrder
    assert(idToOrder(posterOrderId).isState[Fresh] && idToOrder(expectingOrderId).isState[Fresh])

    clock := startAt
    eventWatch.await[OrderFinished](_.key == posterOrderId)
    eventWatch.await[OrderFinished](_.key == expectingOrderId)
  }

  "Order.ExpectingNotice is suspendible" in {
    val qualifier = "2222-11-11"
    val postingOrderId = OrderId(s"#$qualifier#SUSPENDIBLE-POSTING")
    val expectingOrderId = OrderId(s"#$qualifier#SUSPENDIBLE-EXPECTING")
    controllerApi.addOrder(FreshOrder(expectingOrderId, expecting1Workflow.path))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticeExpected](_.key == expectingOrderId)

    controllerApi.executeCommand(SuspendOrders(Seq(expectingOrderId))).await(99.s).orThrow
    eventWatch.await[OrderSuspensionMarked](_.key == expectingOrderId)

    val eventId = eventWatch.lastAddedEventId
    controllerApi.addOrder(FreshOrder(postingOrderId, posting1Workflow.path))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticeRead](_.key == expectingOrderId)
    eventWatch.await[OrderMoved](_.key == expectingOrderId, after = eventId)
    eventWatch.await[OrderSuspended](_.key == expectingOrderId, after = eventId)

    controllerApi.executeCommand(ResumeOrder(expectingOrderId)).await(99.s).orThrow
    eventWatch.await[OrderFinished](_.key == expectingOrderId)
  }

  "Order.ExpectingNotice is cancelable" in {
    val qualifier = "2222-12-12"
    val expectingOrderId = OrderId(s"#$qualifier#CANCELABLE-EXPECTING")
    controllerApi.addOrder(FreshOrder(expectingOrderId, expecting1Workflow.path))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticeExpected](_.key == expectingOrderId)

    controllerApi.executeCommand(CancelOrders(Seq(expectingOrderId))).await(99.s).orThrow
    eventWatch.await[OrderCancelled](_.key == expectingOrderId)
  }

  "Update Board" in {
    val boardState = controllerState.pathToBoardState(board0.path)

    val updatedBoard = board0.copy(postOrderToNoticeId = expr("$jsOrderId"))
    controllerApi.updateUnsignedSimpleItems(Seq(updatedBoard)).await(99.s).orThrow

    assert(controllerState.pathToBoardState(board0.path) ==
      boardState.copy(
        board = updatedBoard.withRevision(Some(ItemRevision(1)))))
  }

  "Delete Board" in {
    val checked = controller
      .updateItemsAsSystemUser(Observable(
        DeleteSimple(board0.path),
        DeleteSimple(board1.path),
        DeleteSimple(board2.path)))
      .await(99.s)
    assert(checked == Left(Problem.Combined(Set(
      ItemIsStillReferencedProblem(board0.path, posting1Workflow.id),
      ItemIsStillReferencedProblem(board0.path, posting3Workflow.id),
      ItemIsStillReferencedProblem(board1.path, posting3Workflow.id),
      ItemIsStillReferencedProblem(board2.path, posting3Workflow.id),
      ItemIsStillReferencedProblem(board0.path, expecting1Workflow.id),
      ItemIsStillReferencedProblem(board0.path, expecting3Workflow.id),
      ItemIsStillReferencedProblem(board1.path, expecting3Workflow.id),
      ItemIsStillReferencedProblem(board2.path, expecting3Workflow.id),
      ItemIsStillReferencedProblem(board0.path, postingAgentWorkflow.id),
      ItemIsStillReferencedProblem(board0.path, expectingAgentWorkflow.id)))))

    controllerApi
      .deleteOrdersWhenTerminated(Observable.fromIterable(
        controllerState.idToOrder.keys))
      .await(99.s).orThrow
    controllerApi
      .updateItems(Observable(
        DeleteSimple(board0.path),
        DeleteSimple(board1.path),
        DeleteSimple(board2.path),
        AddVersion(VersionId("DELETION")),
        RemoveVersioned(posting1Workflow.path),
        RemoveVersioned(posting3Workflow.path),
        RemoveVersioned(expecting1Workflow.path),
        RemoveVersioned(expecting3Workflow.path),
        RemoveVersioned(postingAgentWorkflow.path),
        RemoveVersioned(expectingAgentWorkflow.path)))
      .await(99.s).orThrow

    assert(!controllerState.pathToBoardState.contains(board0.path))
  }
}

object BoardTest
{
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  private val qualifiers = Seq("2222-01-01", "2222-02-02", "2222-03-03")
  private val noticeIds = qualifiers.map(NoticeId(_))
  private val nextQualifier = qualifiers.iterator.next _

  // One lifeTime per board
  private val lifeTimes = Seq(1.days, 2.days, 3.days)
  private val startTimestamp = Timestamp("2222-10-10T00:00:00Z")
  private val endOfLifes = lifeTimes.map(BoardTest.startTimestamp + _)
  private val Seq(endOfLife0, _, _) = endOfLifes

  private val orderIdToNoticeId = expr(
    """replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')""")

  private val boards = for ((lifeTime, i) <- lifeTimes.zipWithIndex) yield
    Board(
      BoardPath(s"BOARD-$i"),
      postOrderToNoticeId = orderIdToNoticeId,
      endOfLife = expr(s"$$js7EpochMilli + ${lifeTime.toMillis}"),
      expectOrderToNoticeId = orderIdToNoticeId)

  private val Seq(board0, board1, board2) = boards

  private val expecting1Workflow = Workflow(WorkflowPath("EXPECTING-1") ~ "INITIAL", Seq(
    ExpectNotice(board0.path)))

  private val expecting3Workflow = Workflow(WorkflowPath("EXPECTING-3") ~ "INITIAL", Seq(
    ExpectNotice(board0.path),
    ExpectNotice(board1.path),
    ExpectNotice(board2.path)))

  private val posting1Workflow = Workflow(WorkflowPath("POSTING-1") ~ "INITIAL", Seq(
    PostNotices(Seq(board0.path))))

  private val posting3Workflow = Workflow(WorkflowPath("POSTING-3") ~ "INITIAL", Seq(
    PostNotices(boards.map(_.path))))

  private val postingAgentWorkflow = Workflow(WorkflowPath("POSTING-AT-AGENT") ~ "INITIAL", Seq(
    EmptyJob.execute(agentPath),
    PostNotices(Seq(board0.path))))

  private val expectingAgentWorkflow = Workflow(WorkflowPath("EXPECTING-AT-AGENT") ~ "INITIAL", Seq(
    EmptyJob.execute(agentPath),
    ExpectNotice(board0.path)))
}
