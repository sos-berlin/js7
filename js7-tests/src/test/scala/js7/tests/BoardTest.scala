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
import js7.data.board.{Board, BoardPath, Notice, NoticeId}
import js7.data.item.ItemOperation.{AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.VersionId
import js7.data.order.Order.Fresh
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderNoticeExpected, OrderNoticePosted, OrderNoticeRead, OrderProcessed, OrderProcessingStarted, OrderStarted}
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

  "Post a notice, then read it" in {
    val qualifier = nextQualifier()
    val notice = Notice(NoticeId(qualifier), endOfLife)

    val posterEvents = controller.runOrder(FreshOrder(OrderId(s"#$qualifier#POSTING"), postingWorkflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(postingWorkflow.id),
      OrderStarted,
      OrderNoticePosted(notice),
      OrderMoved(Position(1)),
      OrderFinished))

    val readerEvents = controller.runOrder(FreshOrder(OrderId(s"#$qualifier#EXPECTING"), expectingWorkflow.path))
    assert(readerEvents.map(_.value) == Seq(
      OrderAdded(expectingWorkflow.id),
      OrderStarted,
      OrderNoticeRead,
      OrderMoved(Position(1)),
      OrderFinished))
  }

  "Expect a notice, then post it" in {
    val qualifier = nextQualifier()
    val notice = Notice(NoticeId(qualifier), endOfLife)

    val expectingOrderId = OrderId(s"#$qualifier#EXPECTING")
    controllerApi.addOrder(FreshOrder(expectingOrderId, expectingWorkflow.path))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderNoticeExpected](_.key == expectingOrderId)

    val posterEvents = controller.runOrder(
      FreshOrder(OrderId(s"#$qualifier#POSTING"), postingWorkflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(postingWorkflow.id),
      OrderStarted,
      OrderNoticePosted(notice),
      OrderMoved(Position(1)),
      OrderFinished))

    controller.eventWatch.await[OrderFinished](_.key == expectingOrderId)
    val expectingEvents = controller.eventWatch.keyedEvents[OrderCoreEvent](expectingOrderId)
    assert(expectingEvents == Seq(
      OrderAdded(expectingWorkflow.id),
      OrderStarted,
      OrderNoticeExpected(notice.id),
      OrderNoticeRead,
      OrderMoved(Position(1)),
      OrderFinished))
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

  "PostNotice and ExpectNotice respect Order.scheduledFor" in {
    val qualifier = nextQualifier()
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

  "Delete notice after endOfLife" in {
    pending
    alarmClock := endOfLife - 1.s
    sleep(100.ms)
    val eventId = controller.eventWatch.lastAddedEventId
    // NoticeDeleted do not occur before endOfLife
    alarmClock := endOfLife
    for (noticeId <- noticeIds) {
      controller.eventWatch.await[NoticeDeleted](_.event.noticeId == noticeId, after = eventId)
    }
  }

  "Update Board" in {
    val updatedBoard = board.copy(postOrderToNoticeId = expr("$jsOrderId"))
    controllerApi.updateUnsignedSimpleItems(Seq(updatedBoard)).await(99.s).orThrow
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
        controller.controllerState.await(99.s).idToOrder.keys))
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
  }
}

object BoardTest
{
  private val agentPath = AgentPath("AGENT")

  private val qualifiers = Seq("2222-01-01", "2222-02-02", "2222-03-03", "2222-04-04")
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
