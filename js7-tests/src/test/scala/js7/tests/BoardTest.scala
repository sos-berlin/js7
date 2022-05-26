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
import js7.data.board.BoardPathExpressionParser.boardPathExpr
import js7.data.board.{Board, BoardPath, BoardState, Notice, NoticeExpectation, NoticeId, NoticePlace}
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{CancelOrders, DeleteNotice, ResumeOrder, SuspendOrders}
import js7.data.item.ItemOperation.{AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.order.Order.Fresh
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderNoticePosted, OrderNoticesExpected, OrderNoticesRead, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderSuspended, OrderSuspensionMarked}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{ExpectNotices, PostNotices}
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
    posting0Workflow, expecting0Workflow,
    posting12Workflow, expecting012Workflow,
    expecting01Workflow, expecting02Workflow,
    postingAgentWorkflow, expectingAgentWorkflow)

  private val clock = TestAlarmClock(startTimestamp)

  override protected def controllerModule = new AbstractModule {
    @Provides @Singleton def provideAlarmClock(): AlarmClock = clock
  }

  "expect(0, 1), post(1, 2), expect(0, 2), post(0)" in {
    val qualifier = nextQualifier()
    val notices = for ((boardPath, endOfLife) <- boards.map(_.path) zip endOfLifes) yield
      Notice(NoticeId(qualifier), boardPath, endOfLife)
    val Seq(notice0, notice1, notice2) = notices

    val expecting01OrderIds = Seq(
      OrderId(s"#$qualifier#EXPECTING-0-1-A"),
      OrderId(s"#$qualifier#EXPECTING-0-1-B"))
    controllerApi.addOrders(
      Observable.fromIterable(expecting01OrderIds).map(FreshOrder(_, expecting01Workflow.path))
    ).await(99.s).orThrow
    for (orderId <- expecting01OrderIds) eventWatch.await[OrderNoticesExpected](_.key == orderId)

    controller.runOrder(FreshOrder(OrderId(s"#$qualifier#POSTING-1-2"), posting12Workflow.path))

    assert(controllerState.pathToBoardState == Map(
      board0.path -> BoardState(
        board0.withRevision(Some(ItemRevision(0))),
        Map(
          notice0.id -> NoticePlace(None, Some(NoticeExpectation(notice0.id, expecting01OrderIds.toSet))))),
      board1.path -> BoardState(
        board1.withRevision(Some(ItemRevision(0))),
        Map(
          notice1.id -> NoticePlace(Some(notice1), Some(NoticeExpectation(notice0.id, expecting01OrderIds.toSet))))),
      board2.path -> BoardState(
        board2.withRevision(Some(ItemRevision(0))),
        Map(
          notice2.id -> NoticePlace(Some(notice2))))))

    controller.runOrder(FreshOrder(OrderId(s"#$qualifier#POSTING-0"), posting0Workflow.path))

    for (orderId <- expecting01OrderIds) {
      eventWatch.await[OrderFinished](_.key == orderId)
      val expectingEvents = eventWatch.eventsByKey[OrderCoreEvent](orderId)
      assert(expectingEvents == Seq(
        OrderAdded(expecting01Workflow.id),
        OrderStarted,
        OrderNoticesExpected(Vector(
          OrderNoticesExpected.Expected(notice0.boardPath, notice0.id),
          OrderNoticesExpected.Expected(notice1.boardPath, notice1.id))),
        OrderNoticesRead,
        OrderMoved(Position(1)),
        OrderFinished))
    }

    val readerEvents = controller.runOrder(FreshOrder(OrderId(s"#$qualifier#EXPECTING-0-2"), expecting02Workflow.path))
    assert(readerEvents.map(_.value) == Seq(
      OrderAdded(expecting02Workflow.id),
      OrderStarted,
      OrderNoticesRead,
      OrderMoved(Position(1)),
      OrderFinished))

    assert(controllerState.pathToBoardState == Map(
      board0.path -> BoardState(
        board0.withRevision(Some(ItemRevision(0))),
        Map(notice0.id -> NoticePlace(Some(notice0)))),
      board1.path -> BoardState(
        board1.withRevision(Some(ItemRevision(0))),
        Map(notice1.id -> NoticePlace(Some(notice1)))),
      board2.path -> BoardState(
        board2.withRevision(Some(ItemRevision(0))),
        Map(notice2.id -> NoticePlace(Some(notice2))))))
  }

  "Two orders expect a notice, then post the notice" in {
    val qualifier = nextQualifier()
    val notice = Notice(NoticeId(qualifier), board0.path, endOfLife0)

    val expectingOrderIds = Seq(OrderId(s"#$qualifier#EXPECTING-A"), OrderId(s"#$qualifier#EXPECTING-B"))
    controllerApi.addOrders(
      Observable.fromIterable(expectingOrderIds).map(FreshOrder(_, expecting0Workflow.path))
    ).await(99.s).orThrow
    for (orderId <- expectingOrderIds) eventWatch.await[OrderNoticesExpected](_.key == orderId)

    val posterEvents = controller.runOrder(
      FreshOrder(OrderId(s"#$qualifier#POSTING"), posting0Workflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(posting0Workflow.id),
      OrderStarted,
      OrderNoticePosted(notice),
      OrderMoved(Position(1)),
      OrderFinished))

    assert(controllerState.pathToBoardState(board0.path) ==
      BoardState(
        board0.withRevision(Some(ItemRevision(0))),
        Map(
          NoticeId("2222-01-01") -> NoticePlace(Some(Notice(NoticeId("2222-01-01"), board0.path, endOfLife0))),  // from previous test
          notice.id -> NoticePlace(Some(notice)))))

    for (orderId <- expectingOrderIds) {
      eventWatch.await[OrderFinished](_.key == orderId)
      val expectingEvents = eventWatch.eventsByKey[OrderCoreEvent](orderId)
      assert(expectingEvents == Seq(
        OrderAdded(expecting0Workflow.id),
        OrderStarted,
        OrderNoticesExpected(Vector(OrderNoticesExpected.Expected(notice.boardPath, notice.id))),
        OrderNoticesRead,
        OrderMoved(Position(1)),
        OrderFinished))
    }

    assert(controllerState.pathToBoardState(board0.path) ==
      BoardState(
        board0.withRevision(Some(ItemRevision(0))),
        View(
          NoticePlace(Some(Notice(NoticeId("2222-01-01"), board0.path, endOfLife0))),  // from previous test
          NoticePlace(Some(notice))
        ).toKeyedMap(_.noticeId)))
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
      OrderNoticesRead,
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
    for (orderId <- orderIds) eventWatch.await[OrderNoticesExpected](_.key == orderId)
    controllerApi.executeCommand(
      ControllerCommand.PostNotice(board0.path, noticeId)
    ).await(99.s).orThrow
    for (orderId <- orderIds) eventWatch.await[OrderNoticesRead](_.key == orderId)

    val notice2 = Notice(NoticeId("2222-08-09"), board0.path, endOfLife0)
    controllerApi.executeCommand(
      ControllerCommand.PostNotice(board0.path, notice2.id)
    ).await(99.s).orThrow
    sleep(100.ms)
    assert(controllerState.pathToBoardState(board0.path).idToNotice(notice2.id) == NoticePlace(Some(notice2)))
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
    assert(controllerState.pathToBoardState(board0.path).idToNotice(notice.id) == NoticePlace(Some(notice)))
    assert(controllerState.pathToBoardState(board0.path).idToNotice(notice2.id) == NoticePlace(Some(notice2)))
  }

  "DeleteNotice command" in {
    val qualifier = "2222-09-09"
    val notice = Notice(NoticeId(qualifier), board0.path, endOfLife0)

    val posterEvents = controller.runOrder(
      FreshOrder(OrderId(s"#$qualifier#POSTING"), posting0Workflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(posting0Workflow.id),
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

  "PostNotices and ExpectNotices respect Order.scheduledFor" in {
    val qualifier = "2222-10-10"
    val posterOrderId = OrderId(s"#$qualifier#POSTER")
    val expectingOrderId = OrderId(s"#$qualifier#EXPECTING")
    val startAt = startTimestamp + 10.days

    controllerApi
      .addOrders(Observable(
        FreshOrder(posterOrderId, posting0Workflow.path, scheduledFor = Some(startAt)),
        FreshOrder(expectingOrderId, expecting0Workflow.path, scheduledFor = Some(startAt))))
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
    controllerApi.addOrder(FreshOrder(expectingOrderId, expecting0Workflow.path))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticesExpected](_.key == expectingOrderId)

    controllerApi.executeCommand(SuspendOrders(Seq(expectingOrderId))).await(99.s).orThrow
    eventWatch.await[OrderSuspensionMarked](_.key == expectingOrderId)

    val eventId = eventWatch.lastAddedEventId
    controllerApi.addOrder(FreshOrder(postingOrderId, posting0Workflow.path))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticesRead](_.key == expectingOrderId)
    eventWatch.await[OrderMoved](_.key == expectingOrderId, after = eventId)
    eventWatch.await[OrderSuspended](_.key == expectingOrderId, after = eventId)

    controllerApi.executeCommand(ResumeOrder(expectingOrderId)).await(99.s).orThrow
    eventWatch.await[OrderFinished](_.key == expectingOrderId)
  }

  "Order.ExpectingNotice is cancelable" in {
    val qualifier = "2222-12-12"
    val expectingOrderId = OrderId(s"#$qualifier#CANCELABLE-EXPECTING")
    controllerApi.addOrder(FreshOrder(expectingOrderId, expecting0Workflow.path))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticesExpected](_.key == expectingOrderId)

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
      ItemIsStillReferencedProblem(board0.path, expecting0Workflow.id),
      ItemIsStillReferencedProblem(board0.path, expecting01Workflow.id),
      ItemIsStillReferencedProblem(board0.path, expecting012Workflow.id),
      ItemIsStillReferencedProblem(board0.path, expecting02Workflow.id),
      ItemIsStillReferencedProblem(board0.path, expectingAgentWorkflow.id),
      ItemIsStillReferencedProblem(board0.path, posting0Workflow.id),
      ItemIsStillReferencedProblem(board0.path, postingAgentWorkflow.id),

      ItemIsStillReferencedProblem(board1.path, expecting01Workflow.id),
      ItemIsStillReferencedProblem(board1.path, expecting012Workflow.id),
      ItemIsStillReferencedProblem(board1.path, posting12Workflow.id),

      ItemIsStillReferencedProblem(board2.path, expecting012Workflow.id),
      ItemIsStillReferencedProblem(board2.path, expecting02Workflow.id),
      ItemIsStillReferencedProblem(board2.path, posting12Workflow.id)))))

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
        RemoveVersioned(expecting0Workflow.path),
        RemoveVersioned(expecting01Workflow.path),
        RemoveVersioned(expecting012Workflow.path),
        RemoveVersioned(expecting02Workflow.path),
        RemoveVersioned(expectingAgentWorkflow.path),
        RemoveVersioned(posting0Workflow.path),
        RemoveVersioned(posting12Workflow.path),
        RemoveVersioned(postingAgentWorkflow.path)))
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
  private val Seq(endOfLife0, endOfLife1, endOfLife2) = endOfLifes

  private val orderIdToNoticeId = expr(
    """replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')""")

  private val boards = for ((lifeTime, i) <- lifeTimes.zipWithIndex) yield
    Board(
      BoardPath(s"BOARD-$i"),
      postOrderToNoticeId = orderIdToNoticeId,
      endOfLife = expr(s"$$js7EpochMilli + ${lifeTime.toMillis}"),
      expectOrderToNoticeId = orderIdToNoticeId)

  private val Seq(board0, board1, board2) = boards

  private val expecting0Workflow = Workflow(WorkflowPath("EXPECTING-0") ~ "INITIAL", Seq(
    ExpectNotices(boardPathExpr(
      s"'${board0.path.string}'"))))

  private val expecting012Workflow = Workflow(WorkflowPath("EXPECTING-0-1-2") ~ "INITIAL", Seq(
    ExpectNotices(boardPathExpr(
      s"'${board0.path.string}' && " +
      s"'${board1.path.string}' && " +
      s"'${board2.path.string}'"))))

  private val expecting01Workflow = Workflow(WorkflowPath("EXPECTING-0-1") ~ "INITIAL", Seq(
    ExpectNotices(boardPathExpr(
      s"'${board0.path.string}' && " +
      s"'${board1.path.string}'"))))

  private val expecting02Workflow = Workflow(WorkflowPath("EXPECTING-0-2") ~ "INITIAL", Seq(
    ExpectNotices(boardPathExpr(
      s"'${board0.path.string}' && " +
      s"'${board2.path.string}'"))))

  private val posting0Workflow = Workflow(WorkflowPath("POSTING-0") ~ "INITIAL", Seq(
    PostNotices(Seq(board0.path))))

  private val posting12Workflow = Workflow(WorkflowPath("POSTING-1-2") ~ "INITIAL", Seq(
    PostNotices(Seq(board1.path, board2.path))))

  private val postingAgentWorkflow = Workflow(WorkflowPath("POSTING-AT-AGENT") ~ "INITIAL", Seq(
    EmptyJob.execute(agentPath),
    PostNotices(Seq(board0.path))))

  private val expectingAgentWorkflow = Workflow(WorkflowPath("EXPECTING-AT-AGENT") ~ "INITIAL", Seq(
    EmptyJob.execute(agentPath),
    ExpectNotices(boardPathExpr(s"'${board0.path.string}'"))))
}
