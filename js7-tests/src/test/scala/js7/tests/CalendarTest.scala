package js7.tests

import fs2.Stream
import java.time.ZoneId
import js7.agent.RunningAgent
import js7.base.configutils.Configs.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.time.{TestAlarmClock, Timestamp, Timezone}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.item.BasicItemEvent.{ItemAttached, ItemDeleted, ItemDeletionMarked, ItemDetachable, ItemDetached}
import js7.data.item.ItemOperation.{AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemChanged
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCycleFinished, OrderCycleStarted, OrderCyclingPrepared, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{CycleState, FreshOrder, OrderId, OrderOutcome}
import js7.data.workflow.instructions.{Cycle, Schedule}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.CalendarTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId

final class CalendarTest extends OurTestSuite, ControllerAgentForScalaTest:

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(calendar, workflow)

  private lazy val clock =
    given ZoneId = CalendarTest.zone
    TestAlarmClock(local("2021-10-01T00:00"))

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    #js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  override protected def controllerTestWiring = RunningController.TestWiring(
    alarmClock = Some(clock))

  override protected def agentTestWiring = RunningAgent.TestWiring(
    alarmClock = Some(clock))

  "Reject invalid Calendar" in:
    // Falsches Datumsformat
    // Falscher dateOffset
    val checked = controller.api.updateUnsignedSimpleItems(Seq(calendar.copy(dateOffset = 24.h)))
      .await(99.s)
    assert(checked == Left(Problem("Invalid dateOffset")))

  "Use Calendar" in:
    val eventId = eventWatch.lastAddedEventId
    val events = controller.runOrder(
      FreshOrder(OrderId("#2021-10-01#"), workflow.path, deleteWhenTerminated = true))
    assert(events.map(_.value) == expectedOrderEvents)

    eventWatch.await[ItemAttached](_.event.key == calendar.path, after = eventId)

  "Change Calendar" in:
    val myCalendar = Calendar(
      CalendarPath("CALENDAR"),
      orderIdToDatePattern = "/([^/]+)/.*",
      periodDatePattern = "yyyy-MM-dd")

    val eventId = eventWatch.lastAddedEventId
    controller.api.updateUnsignedSimpleItems(Seq(myCalendar))
      .await(99.s).orThrow
    eventWatch.await[UnsignedSimpleItemChanged](_.event.key == calendar.path, after = eventId)

    val events = controller.runOrder(
      FreshOrder(OrderId("/2021-10-01/"), workflow.path, deleteWhenTerminated = true))
    assert(events.map(_.value) == expectedOrderEvents)

    eventWatch.await[ItemAttached](_.event.key == calendar.path, after = eventId)

  "Delete Workflow and Calendar" in:
    val eventId = eventWatch.lastAddedEventId
    if false then // TODO Allow and test simultaneous deletion of Calendar and Workflow (?)
      controller.api.updateItems(Stream(
        DeleteSimple(calendar.path),
        AddVersion(VersionId("DELETE")),
        RemoveVersioned(workflow.path))
      ).await(99.s).orThrow
    else
      controller.api.updateItems(Stream(
        AddVersion(VersionId("DELETE")),
        RemoveVersioned(workflow.path))
      ).await(99.s).orThrow
      eventWatch.await[ItemDeleted](_.event.key == workflow.id, after = eventId)
      controller.api.updateItems(Stream(
        DeleteSimple(calendar.path)),
      ).await(99.s).orThrow
    eventWatch.await[ItemDeletionMarked](_.event.key == calendar.path, after = eventId)
    eventWatch.await[ItemDetachable](_.event.key == calendar.path, after = eventId)
    eventWatch.await[ItemDetached](_.event.key == calendar.path, after = eventId)
    eventWatch.await[ItemDeleted](_.event.key == calendar.path, after = eventId)


object CalendarTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private given zone: ZoneId = ZoneId.of("Europe/Mariehamn")

  private val calendar = Calendar(
    CalendarPath("CALENDAR"),
    orderIdToDatePattern = "#([^#]+)#.*",
    periodDatePattern = "yyyy-MM-dd")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      Cycle(Schedule.continuous(pause = 1.s, limit = Some(1))):
        Workflow.of:
          EmptyJob.execute(agentPath)),
    timeZone = Timezone(zone.toString),
    calendarPath = Some(calendar.path))

  private val expectedOrderEvents = Seq(
    OrderAdded(workflow.id, deleteWhenTerminated = true),
    OrderStarted,
    OrderCyclingPrepared(CycleState(
      next = Timestamp.Epoch,
      index = 1,
      end = local("2021-10-02T00:00"))),
    OrderCycleStarted,
    OrderAttachable(agentPath),
    OrderAttached(agentPath),
    OrderProcessingStarted(subagentId),
    OrderProcessed(OrderOutcome.succeeded),
    OrderMoved(Position(0) / "cycle+end=1633122000000,i=1" %  1),
    OrderCycleFinished(None),
    OrderMoved(Position(1)),
    OrderDetachable,
    OrderDetached,
    OrderFinished(),
    OrderDeleted)
