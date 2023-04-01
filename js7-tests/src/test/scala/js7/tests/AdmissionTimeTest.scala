package js7.tests

import java.time.DayOfWeek.{MONDAY, SUNDAY}
import java.time.{LocalTime, ZoneId}
import js7.agent.RunningAgent
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.*
import js7.base.time.{AdmissionTimeScheme, TestAlarmClock, Timezone, WeekdayPeriod}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.order.Order.Fresh
import js7.data.order.OrderEvent.{OrderAttached, OrderFinished}
import js7.data.order.OrderObstacle.waitingForAdmmission
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.AdmissionTimeTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.duration.*

final class AdmissionTimeTest extends OurTestSuite with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected def agentPaths = Seq(agentPath)
  protected def items = Seq(mondayWorkflow, sundayWorkflow)

  private implicit val timeZone: ZoneId = AdmissionTimeTest.timeZone
  private implicit val clock: TestAlarmClock = TestAlarmClock(local("2021-03-20T00:00"))

  override protected def agentTestWiring = RunningAgent.TestWiring(
    alarmClock = Some(clock))

  "Sunday at start of daylight saving time" - {
    "Wait for start of permission period" in {
      val orderId = OrderId("🔻")
      val eventId = eventWatch.lastAddedEventId
      controller.api.addOrder(FreshOrder(orderId, sundayWorkflow.path)).await(99.s).orThrow
      eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
      assert(controllerState.idToOrder(orderId).isState[Fresh])
      assert(controllerState.idToOrder(orderId).position == Position(0))
      assert(orderToObstacles(orderId) ==
        Right(Set(waitingForAdmmission(local("2021-03-21T03:00")))))

      assert(orderToObstacles(orderId) ==
        Right(Set(waitingForAdmmission(local("2021-03-21T03:00")))))
      assert(orderObstacleCalculator.waitingForAdmissionOrderCount(clock.now()) == 1)

      clock := local("2021-03-21T02:59")
      sleep(100.ms)
      assert(controllerState.idToOrder(orderId).isState[Fresh])
      assert(controllerState.idToOrder(orderId).position == Position(0))
      assert(orderToObstacles(orderId) ==
        Right(Set(waitingForAdmmission(local("2021-03-21T03:00")))))

      clock := local("2021-03-21T03:00")
      eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
      assert(orderObstacleCalculator.waitingForAdmissionOrderCount(clock.now()) == 0)
    }

    "Start order while in permission period" in {
      clock := local("2021-03-21T03:59")
      val eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("🔺")
      controller.api.addOrder(FreshOrder(orderId, sundayWorkflow.path)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
    }

    "Start order at end of permission period" in {
      clock := local("2021-03-21T04:00")
      val eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("🔸")
      controller.api.addOrder(FreshOrder(orderId, sundayWorkflow.path)).await(99.s).orThrow
      assert(controllerState.idToOrder(orderId).isState[Fresh])
      assert(controllerState.idToOrder(orderId).position == Position(0))
      assert(orderToObstacles(orderId) ==
        Right(Set(waitingForAdmmission(local("2021-03-28T03:00")))))
    }

    "Start order with permission in daylight saving time gap" in {
      // Admission is shifted to the next valid local time
      assert(local("2021-03-28T04:00") - local("2021-03-28T02:59") == 1.minute)
      clock := local("2021-03-28T02:59")
      val eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("🔹")
      controller.api.addOrder(FreshOrder(orderId, sundayWorkflow.path)).await(99.s).orThrow
      eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
      assert(controllerState.idToOrder(orderId).isState[Fresh])
      assert(controllerState.idToOrder(orderId).position == Position(0))
      assert(orderToObstacles(orderId) ==
        Right(Set(waitingForAdmmission(local("2021-03-28T03:00")))))

      clock := local("2021-03-28T04:00")
      eventWatch.await[OrderFinished](_.key == orderId)
    }

    "Start order with permission in daylight saving time gap (2)" in {
      clock := local("2021-03-28T04:59")
      val eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("🔶")
      controller.api.addOrder(FreshOrder(orderId, sundayWorkflow.path)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
    }

    "Start order at end of shifted permission period" in {
      clock := local("2021-03-28T05:00")
      val orderId = OrderId("🔷")
      controller.api.addOrder(FreshOrder(orderId, sundayWorkflow.path)).await(99.s).orThrow
      assert(controllerState.idToOrder(orderId).isState[Fresh])
      assert(controllerState.idToOrder(orderId).position == Position(0))
      assert(orderToObstacles(orderId) ==
        Right(Set(waitingForAdmmission(local("2021-04-04T03:00")))))
    }
  }

  "Monday after end of daylight saving time" in {
    assert(local("2021-10-31T04:00") - local("2021-10-31T03:59") == 1.h + 1.minute)

    clock := local("2021-10-31T00:00")
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("♣️")
    controller.api.addOrder(FreshOrder(orderId, mondayWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderAttached](_.key == orderId)
    assert(controllerState.idToOrder(orderId).isState[Fresh])
    assert(controllerState.idToOrder(orderId).position == Position(0))

    clock := local("2021-11-01T07:59")
    sleep(100.ms)
    assert(controllerState.idToOrder(orderId).isState[Fresh])
    assert(controllerState.idToOrder(orderId).position == Position(0))

    clock := local("2021-11-01T08:00")
    eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
  }

  "Late start" in {
    val tooEarly = local("2021-11-08T07:59")
    assert(!mondayAdmissionTimeScheme.isPermitted(tooEarly, timeZone, dateOffset = 0.s))
    clock := tooEarly
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("♠️")
    controller.api.addOrder(FreshOrder(orderId, mondayWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
    assert(controllerState.idToOrder(orderId).isState[Fresh])
    assert(controllerState.idToOrder(orderId).position == Position(0))

    // Let the clock skip the permission interval until it is too late.
    // This may happen due to any delay, for example due to other other orders in the job.
    assert(mondayAdmissionTimeScheme
      .isPermitted(local("2021-11-08T08:00"), timeZone, dateOffset = 0.s))

    val tooLate = local("2021-11-08T10:00")
    assert(!mondayAdmissionTimeScheme.isPermitted(tooLate, timeZone, dateOffset = 0.s))
    clock := tooLate  // To late
    sleep(100.ms)
    assert(controllerState.idToOrder(orderId).isState[Fresh])
    assert(controllerState.idToOrder(orderId).position == Position(0))
  }
}

object AdmissionTimeTest
{
  private val agentPath = AgentPath("AGENT")
  private val timeZone = ZoneId.of("Europe/Mariehamn")

  private val mondayAdmissionTimeScheme = AdmissionTimeScheme(Seq(
    WeekdayPeriod(MONDAY, LocalTime.of(8, 0), 2.h)))

  private val mondayWorkflow = Workflow(WorkflowPath("MONDAY") ~ "INITIAL",
    Seq(
      Execute(WorkflowJob(agentPath, EmptyJob.executable(),
        admissionTimeScheme = Some(mondayAdmissionTimeScheme)))),
    timeZone = Timezone(timeZone.getId))

  private val sundayAdmissionTimeScheme = AdmissionTimeScheme(Seq(
    WeekdayPeriod(SUNDAY, LocalTime.of(3, 0), 1.h) /*dst gap*/))

  private val sundayWorkflow = Workflow(WorkflowPath("SUNDAY") ~ "INITIAL",
    Seq(
      Execute(WorkflowJob(agentPath, EmptyJob.executable(),
        admissionTimeScheme = Some(sundayAdmissionTimeScheme)))),
    timeZone = Timezone(timeZone.getId))
}
