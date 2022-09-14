package js7.tests

import java.time.{LocalDate, LocalTime, ZoneId}
import js7.base.configutils.Configs.*
import js7.base.log.Logger
import js7.base.test.Test
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.{DurationRichInt, sleep}
import js7.base.time.{AdmissionTimeScheme, DailyPeriod, Timezone}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.execution.workflow.instructions.ScheduleTester
import js7.data.order.OrderEvent.OrderCycleStarted
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.workflow.instructions.Schedule.{Scheme, Ticking}
import js7.data.workflow.instructions.{Cycle, Schedule}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.Cycle2Test.*
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.*

// Repeats the JS-2012 test case in CycleTest with the real wall clock, to be sure.
final class Cycle2Test extends Test with ControllerAgentForScalaTest with ScheduleTester
{
  protected val agentPaths = Nil
  protected val items = Seq(calendar, js2012Workflow)

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  "One first cycle in mid of period (bug JS-2012)" in {
    val now = LocalTime.now()
    val now0 = now.withHour(0)
    if (now.isBefore(LocalTime.of(1, 0))
     || now0.isAfter(LocalTime.of(0, 59)) && now0.isBefore(LocalTime.of(0, 0, 1))
     || now0.isAfter(LocalTime.of(0, 59)) && now0.isBefore(LocalTime.of(0, 0, 1))) {
      logger.warn("Test does not work around a full hour")
      pending
    } else {
      val today = LocalDate.now().toString
      val orderId = OrderId(s"#$today#ONCE-A-DAY")
      controllerApi.addOrder(FreshOrder(orderId, js2012Workflow.path))
        .await(99.s).orThrow
      eventWatch.await[OrderCycleStarted](_.key == orderId)
      sleep(1.s)
      assert(eventWatch.eventsByKey[OrderEvent](orderId).count(_ == OrderCycleStarted) == 1)
    }
  }
}

object Cycle2Test
{
  private val logger = Logger[this.type]
  private implicit val zone: ZoneId = ZoneId.systemDefault

  private val calendar = Calendar.jocStandard(
    CalendarPath("CALENDAR"),
    Timezone(zone.getId),
    dateOffset = 0.h)

  private val js2012Workflow = Workflow(
    WorkflowPath("ONCE-AN-HOUR") ~ "INITIAL",
    Seq(
      Cycle(
        Schedule(Seq(Scheme(
          AdmissionTimeScheme(Seq(
            DailyPeriod(0, 24 * 3600.s))),
          Ticking(1.h)))),
        Workflow.empty)),
    calendarPath = Some(calendar.path))
}
