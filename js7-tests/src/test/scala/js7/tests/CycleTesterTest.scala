package js7.tests

import com.google.inject.{AbstractModule, Provides}
import javax.inject.Singleton
import js7.base.configutils.Configs._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.JavaTime.JavaTimeZone
import js7.base.time.JavaTimestamp.local
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.time.ScalaTime._
import js7.base.time.{AlarmClock, TestAlarmClock, Timestamp, Timezone}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.execution.workflow.instructions.CycleTester
import js7.data.order.OrderEvent.{OrderCycleFinished, OrderCycleStarted, OrderCyclingPrepared, OrderFinished}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.CycleTesterTest._
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.VectorBuilder

/** Test the js7-data CycleTest JSON example. */
final class CycleTesterTest extends AnyFreeSpec with ControllerAgentForScalaTest with CycleTester
{
  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow, calendar)

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  override protected def controllerModule = new AbstractModule {
    @Provides @Singleton def alarmClock(): AlarmClock = clock
  }
  override protected def agentModule = new AbstractModule {
    @Provides @Singleton def alarmClock(): AlarmClock = clock
  }

  private implicit val zone = calendar.timezone.toZoneId.orThrow

  "TestCycle example" - {
    clock.resetTo(local("2021-10-01T00:00"))

    addStandardCycleTests { (timeInterval, cycleDuration, zone, expected, exitTimestamp) =>
      val end = timeInterval.end // ???
      val expectedCycleStartTimes = expected
        .map { case (cycleWaitTimestamp, cycleState) =>
          cycleWaitTimestamp max cycleState.next  // Expected time of OrderCycleStart
        }

      var eventId = eventWatch.lastAddedEventId
      clock.resetTo(timeInterval.start - 1.s)  // Start the order early

      val orderDate = timeInterval.start.toLocalDateTime(zone).toLocalDate
      val orderId = OrderId(s"#$orderDate#CycleTesterTest")
      scribe.debug(s"addOrder $orderId")
      controllerApi.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow

      eventWatch.await[OrderCyclingPrepared](_.key == orderId)
      val cycleStartedTimes = new VectorBuilder[Timestamp]
      for (t <- expectedCycleStartTimes) {
        clock := t  // Difference may be zero, so OrderCycleStarted may already have been emitted
        val stamped = eventWatch
          .await[OrderCycleStarted](_.key == orderId, after = eventId)
          .head
        cycleStartedTimes += stamped.timestamp
        eventId = stamped.eventId

        clock += cycleDuration
        TestJob.continue.runSyncUnsafe()
        eventId = eventWatch.await[OrderCycleFinished](_.key == orderId, after = eventId)
          .head.eventId
      }
      assert(cycleStartedTimes.result() == expectedCycleStartTimes)

      clock := exitTimestamp
      eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
    }
  }
}

object CycleTesterTest
{
  private val clock = TestAlarmClock(Timestamp("2021-01-01T00:00:00Z"))
  private val agentPath = AgentPath("AGENT")

  // Use this Log4j Clock with the properties
  // -Dlog4j2.Clock=js7.tests.CycleTesterTest$CycleTestLog4jClock -Duser.timezone=Europe/Mariehamn
  final class CycleTestLog4jClock extends org.apache.logging.log4j.core.util.Clock
  {
    def currentTimeMillis() = clock.epochMilli()
  }

  private val calendar = Calendar(
    CalendarPath("CALENDAR"),
    Timezone(CycleTester.zoneId.getId),
    dateOffset = 0.h,  // FIXME Test with dateOffset = 6.h
    orderIdToDatePattern = "#([^#]+)#.*",
    periodDatePattern = "yyyy-MM-dd")

  private val workflow =
    Workflow(WorkflowPath("CycleTesterTest"),
      Seq(
        js7.data.workflow.instructions.CycleTest.exampleCycle
          .copy(
            cycleWorkflow = Workflow.of(
              TestJob.execute(agentPath)))),
      calendarPath = Some(calendar.path))

  final class TestJob extends SemaphoreJob(TestJob)
  object TestJob extends SemaphoreJob.Companion[TestJob]
}
