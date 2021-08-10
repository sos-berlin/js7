package js7.tests

import com.google.inject.{AbstractModule, Provides}
import javax.inject.Singleton
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.time.{AlarmClock, Timestamp}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.Problems.ItemIsStillReferencedProblem
import js7.data.agent.AgentPath
import js7.data.board.BoardEvent.NoticeDeleted
import js7.data.board.{Board, BoardPath, BoardState, Notice, NoticeId}
import js7.data.controller.ControllerCommand.{CancelOrders, DeleteNotice, ResumeOrder, SuspendOrders}
import js7.data.item.ItemOperation.{AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.order.Order.Fresh
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderNoticeExpected, OrderNoticePosted, OrderNoticeRead, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderSuspended, OrderSuspensionMarked}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{ExpectNotice, PostNotice}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.BoardTest._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
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

  protected def items = Seq(board,
    postingWorkflow, expectingWorkflow,
    postingAgentWorkflow, expectingAgentWorkflow)

  private val alarmClock = AlarmClock.forTest(startTimestamp, clockCheckInterval = 100.ms)

  override protected def controllerModule = new AbstractModule {
    @Provides @Singleton def provideAlarmClock(): AlarmClock = alarmClock
  }

  "Post a notice, then expect it" in {
    val qualifier = nextQualifier()
    val notice = Notice(NoticeId(qualifier), endOfLife)

    val posterEvents = controller.runOrder(FreshOrder(OrderId(s"#$qualifier#POSTING"), postingWorkflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(postingWorkflow.id),
      OrderStarted,
      OrderNoticePosted(notice),
      OrderMoved(Position(1)),
      OrderFinished))

    assert(controllerState.pathToBoardState(board.path) ==
      BoardState(
        board.withRevision(Some(ItemRevision(0))),
        Map(notice.id -> notice)))

    val readerEvents = controller.runOrder(FreshOrder(OrderId(s"#$qualifier#EXPECTING"), expectingWorkflow.path))
    assert(readerEvents.map(_.value) == Seq(
      OrderAdded(expectingWorkflow.id),
      OrderStarted,
      OrderNoticeRead,
      OrderMoved(Position(1)),
      OrderFinished))

    assert(controllerState.pathToBoardState(board.path) ==
      BoardState(
        board.withRevision(Some(ItemRevision(0))),
        Map(notice.id -> notice)))
  }

  "Expect a notice, then post it" in {
    val qualifier = nextQualifier()
    val notice = Notice(NoticeId(qualifier), endOfLife)

    val expectingOrderId = OrderId(s"#$qualifier#EXPECTING")
    controllerApi.addOrder(FreshOrder(expectingOrderId, expectingWorkflow.path))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticeExpected](_.key == expectingOrderId)

    val posterEvents = controller.runOrder(
      FreshOrder(OrderId(s"#$qualifier#POSTING"), postingWorkflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(postingWorkflow.id),
      OrderStarted,
      OrderNoticePosted(notice),
      OrderMoved(Position(1)),
      OrderFinished))

    assert(controllerState.pathToBoardState(board.path) ==
      BoardState(
        board.withRevision(Some(ItemRevision(0))),
        Map(
          NoticeId("2222-01-01") -> Notice(NoticeId("2222-01-01"), endOfLife),  // from previous test
          notice.id -> notice)))

    eventWatch.await[OrderFinished](_.key == expectingOrderId)
    val expectingEvents = eventWatch.keyedEvents[OrderCoreEvent](expectingOrderId)
    assert(expectingEvents == Seq(
      OrderAdded(expectingWorkflow.id),
      OrderStarted,
      OrderNoticeExpected(notice.id),
      OrderNoticeRead,
      OrderMoved(Position(1)),
      OrderFinished))

    assert(controllerState.pathToBoardState(board.path) ==
      BoardState(
        board.withRevision(Some(ItemRevision(0))),
        Map(
          NoticeId("2222-01-01") -> Notice(NoticeId("2222-01-01"), endOfLife),  // from previous test
          notice.id -> notice)))
  }

  "Detach order when at Agent" in {
    // TODO Post kann am Agenten ausgeführt werden, wenn Board (ohne BoardState) dahin übertragen wird,
    //  und anschließend der Controller Order.ExpectingNotice löst.
    val qualifier = nextQualifier()
    val notice = Notice(NoticeId(qualifier), endOfLife)

    val posterEvents = controller.runOrder(
      FreshOrder(OrderId(s"#$qualifier#POSTING"), postingAgentWorkflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(postingAgentWorkflow.id),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted,
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
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderNoticeRead,
      OrderMoved(Position(2)),
      OrderFinished))
  }

  "Delete notice after endOfLife" in {
    alarmClock := endOfLife - 1.s
    sleep(100.ms)
    val eventId = eventWatch.lastAddedEventId
    // NoticeDeleted do not occur before endOfLife
    alarmClock := endOfLife
    // Spare noticeIds.head for DeleteNotice test
    for (noticeId <- noticeIds) {
      eventWatch.await[NoticeDeleted](_.event.noticeId == noticeId, after = eventId)
    }
  }

  "DeleteNotice command" in {
    val qualifier = "2222-08-08"
    val notice = Notice(NoticeId(qualifier), endOfLife + lifeTime)

    val posterEvents = controller.runOrder(
      FreshOrder(OrderId(s"#$qualifier#POSTING"), postingWorkflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(postingWorkflow.id),
      OrderStarted,
      OrderNoticePosted(notice),
      OrderMoved(Position(1)),
      OrderFinished))

    val eventId = eventWatch.lastAddedEventId
    controllerApi.executeCommand(DeleteNotice(board.path, notice.id)).await(99.s).orThrow
    assert(eventWatch.await[NoticeDeleted](_.key == board.path, after = eventId).head.value.event ==
      NoticeDeleted(notice.id))
    sleep(100.ms)
  }

  "PostNotice and ExpectNotice respect Order.scheduledFor" in {
    val qualifier = "2222-09-09"
    val posterOrderId = OrderId(s"#$qualifier#POSTER")
    val expectingOrderId = OrderId(s"#$qualifier#EXPECTING")
    val startAt = startTimestamp + 10.days

    controllerApi
      .addOrders(Observable(
        FreshOrder(posterOrderId, postingWorkflow.path, scheduledFor = Some(startAt)),
        FreshOrder(expectingOrderId, expectingWorkflow.path, scheduledFor = Some(startAt))))
      .await(99.s).orThrow
    sleep(500.ms)
    val idToOrder = controllerApi.controllerState.await(99.s).orThrow.idToOrder
    assert(idToOrder(posterOrderId).isState[Fresh] && idToOrder(expectingOrderId).isState[Fresh])

    alarmClock := startAt
    eventWatch.await[OrderFinished](_.key == posterOrderId)
    eventWatch.await[OrderFinished](_.key == expectingOrderId)
  }

  "Order.ExpectingNotice is suspendible" in {
    val qualifier = "2222-10-10"
    val postingOrderId = OrderId(s"#$qualifier#SUSPENDIBLE-POSTING")
    val expectingOrderId = OrderId(s"#$qualifier#SUSPENDIBLE-EXPECTING")
    controllerApi.addOrder(FreshOrder(expectingOrderId, expectingWorkflow.path))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticeExpected](_.key == expectingOrderId)

    controllerApi.executeCommand(SuspendOrders(Seq(expectingOrderId))).await(99.s).orThrow
    eventWatch.await[OrderSuspensionMarked](_.key == expectingOrderId)

    val eventId = eventWatch.lastAddedEventId
    controllerApi.addOrder(FreshOrder(postingOrderId, postingWorkflow.path))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticeRead](_.key == expectingOrderId)
    eventWatch.await[OrderMoved](_.key == expectingOrderId, after = eventId)
    eventWatch.await[OrderSuspended](_.key == expectingOrderId, after = eventId)

    controllerApi.executeCommand(ResumeOrder(expectingOrderId)).await(99.s).orThrow
    eventWatch.await[OrderFinished](_.key == expectingOrderId)
  }

  "Order.ExpectingNotice is cancelable" in {
    val qualifier = "2222-11-11"
    val expectingOrderId = OrderId(s"#$qualifier#CANCELABLE-EXPECTING")
    controllerApi.addOrder(FreshOrder(expectingOrderId, expectingWorkflow.path))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticeExpected](_.key == expectingOrderId)

    controllerApi.executeCommand(CancelOrders(Seq(expectingOrderId))).await(99.s).orThrow
    eventWatch.await[OrderCancelled](_.key == expectingOrderId)
  }

  "Update Board" in {
    val boardState = controllerState.pathToBoardState(board.path)

    val updatedBoard = board.copy(postOrderToNoticeId = expr("$jsOrderId"))
    controllerApi.updateUnsignedSimpleItems(Seq(updatedBoard)).await(99.s).orThrow

    assert(controllerState.pathToBoardState(board.path) ==
      boardState.copy(
        board = updatedBoard.withRevision(Some(ItemRevision(1)))))
  }

  "Delete Board" in {
    val checked = controllerApi.updateItems(Observable(DeleteSimple(board.path)))
      .await(99.s)
    assert(checked == Left(Problem.Combined(Set(
      ItemIsStillReferencedProblem(board.path, postingWorkflow.id),
      ItemIsStillReferencedProblem(board.path, expectingWorkflow.id),
      ItemIsStillReferencedProblem(board.path, postingAgentWorkflow.id),
      ItemIsStillReferencedProblem(board.path, expectingAgentWorkflow.id)))))

    controllerApi
      .deleteOrdersWhenTerminated(Observable.fromIterable(
        controllerState.idToOrder.keys))
      .await(99.s).orThrow
    controllerApi
      .updateItems(Observable(
        DeleteSimple(board.path),
        AddVersion(VersionId("DELETION")),
        RemoveVersioned(postingWorkflow.path),
        RemoveVersioned(expectingWorkflow.path),
        RemoveVersioned(postingAgentWorkflow.path),
        RemoveVersioned(expectingAgentWorkflow.path)))
      .await(99.s).orThrow

    assert(!controllerState.pathToBoardState.contains(board.path))
  }
}

object BoardTest
{
  private val agentPath = AgentPath("AGENT")

  private val qualifiers = Seq("2222-01-01", "2222-02-02", "2222-03-03")
  private val noticeIds = qualifiers.map(NoticeId(_))
  private val nextQualifier = qualifiers.iterator.next _

  private val lifeTime = 2.days
  private val startTimestamp = Timestamp("2222-11-11T00:00:00Z")
  private val endOfLife = BoardTest.startTimestamp + lifeTime

  private val orderIdToNoticeId = expr(
    """replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')""")

  private val board = Board(
    BoardPath("BOARD"),
    postOrderToNoticeId = orderIdToNoticeId,
    endOfLife = expr(s"$$epochMilli + ${lifeTime.toMillis}"),
    expectOrderToNoticeId = orderIdToNoticeId)

  private val expectingWorkflow = Workflow(WorkflowPath("EXPECTING") ~ "INITIAL", Seq(
    ExpectNotice(board.path)))

  private val postingWorkflow = Workflow(WorkflowPath("POSTING") ~ "INITIAL", Seq(
    PostNotice(board.path)))

  private val postingAgentWorkflow = Workflow(WorkflowPath("POSTING-AT-AGENT") ~ "INITIAL", Seq(
    EmptyJob.execute(agentPath),
    PostNotice(board.path)))

  private val expectingAgentWorkflow = Workflow(WorkflowPath("EXPECTING-AT-AGENT") ~ "INITIAL", Seq(
    EmptyJob.execute(agentPath),
    ExpectNotice(board.path)))
}
