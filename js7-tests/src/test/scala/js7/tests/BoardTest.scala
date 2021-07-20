package js7.tests

import com.google.inject.{AbstractModule, Provides}
import javax.inject.Singleton
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.time.{AlarmClock, Timestamp}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.board.BoardEvent.NoticeDeleted
import js7.data.board.{Board, BoardPath, Notice, NoticeId}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderNoticeAwaiting, OrderNoticePosted, OrderNoticeRead, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{PostNotice, ReadNotice}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.BoardTest._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

final class BoardTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected def agentPaths = Seq(agentPath)

  protected def items = Seq(board,
    readerWorkflow, posterWorkflow,
    readerAgentWorkflow, posterAgentWorkflow)

  private val alarmClock = AlarmClock.forTest(startTimestamp, clockCheckInterval = 100.ms)

  override protected def controllerModule = new AbstractModule {
    @Provides @Singleton def provideAlarmClock(): AlarmClock = alarmClock
  }

  "Post a notice, then read it" in {
    val qualifier = nextQualifier()
    val notice = Notice(NoticeId(qualifier), endOfLife)

    val posterEvents = controller.runOrder(FreshOrder(OrderId(s"#$qualifier#POSTER"), posterWorkflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(posterWorkflow.id),
      OrderStarted,
      OrderNoticePosted(notice),
      OrderMoved(Position(1)),
      OrderFinished))

    val readerEvents = controller.runOrder(FreshOrder(OrderId(s"#$qualifier#READER"), readerWorkflow.path))
    assert(readerEvents.map(_.value) == Seq(
      OrderAdded(readerWorkflow.id),
      OrderStarted,
      OrderNoticeRead,
      OrderMoved(Position(1)),
      OrderFinished))
  }

  "Wait for a notice, then post it" in {
    val qualifier = nextQualifier()
    val notice = Notice(NoticeId(qualifier), endOfLife)

    val readerOrderId = OrderId(s"#$qualifier#READER")
    controllerApi.addOrder(FreshOrder(readerOrderId, readerWorkflow.path))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderNoticeAwaiting](_.key == readerOrderId)

    val posterEvents = controller.runOrder(
      FreshOrder(OrderId(s"#$qualifier#POSTER"), posterWorkflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(posterWorkflow.id),
      OrderStarted,
      OrderNoticePosted(notice),
      OrderMoved(Position(1)),
      OrderFinished))

    controller.eventWatch.await[OrderFinished](_.key == readerOrderId)
    val readerEvents = controller.eventWatch.keyedEvents[OrderCoreEvent](readerOrderId)
    assert(readerEvents == Seq(
      OrderAdded(readerWorkflow.id),
      OrderStarted,
      OrderNoticeAwaiting(notice.id),
      OrderNoticeRead,
      OrderMoved(Position(1)),
      OrderFinished))
  }

  "Detach order when at Agent" in {
    // TODO Post kann am Agenten ausgeführt werden, wenn Board (ohne BoardState) dahin übertragen wird,
    //  und anschließend der Controller Order.WaitingForNotice löst.
    val qualifier = nextQualifier()
    val notice = Notice(NoticeId(qualifier), endOfLife)

    val posterEvents = controller.runOrder(
      FreshOrder(OrderId(s"#$qualifier#POSTER"), posterAgentWorkflow.path))
    assert(posterEvents.map(_.value) == Seq(
      OrderAdded(posterAgentWorkflow.id),
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
      FreshOrder(OrderId(s"#$qualifier#READER"), readerAgentWorkflow.path))
    assert(readerEvents.map(_.value) == Seq(
      OrderAdded(readerAgentWorkflow.id),
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
    val eventId = controller.eventWatch.lastAddedEventId
    // NoticeDeleted do not occur before endOfLife
    alarmClock := endOfLife
    for (noticeId <- noticeIds) {
      controller.eventWatch.await[NoticeDeleted](_.event.noticeId == noticeId, after = eventId)
    }
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
    toNotice = orderIdToNoticeId,
    endOfLife = expr(s"$$epochMilli + ${lifeTime.toMillis}"),
    readingOrderToNoticeId = orderIdToNoticeId)

  private val readerWorkflow = Workflow(WorkflowPath("READER") ~ "INITIAL", Seq(
    ReadNotice(board.path)))

  private val posterWorkflow = Workflow(WorkflowPath("POSTER") ~ "INITIAL", Seq(
    PostNotice(board.path)))

  private val readerAgentWorkflow = Workflow(WorkflowPath("READER-AGENT") ~ "INITIAL", Seq(
    EmptyJob.execute(agentPath),
    ReadNotice(board.path)))

  private val posterAgentWorkflow = Workflow(WorkflowPath("POSTER-AGENT") ~ "INITIAL", Seq(
    EmptyJob.execute(agentPath),
    PostNotice(board.path)))
}
