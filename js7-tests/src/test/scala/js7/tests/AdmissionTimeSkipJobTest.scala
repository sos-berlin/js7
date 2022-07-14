package js7.tests

import com.google.inject.{AbstractModule, Provides}
import java.time.DayOfWeek.{FRIDAY, THURSDAY}
import java.time.{LocalTime, ZoneId}
import javax.inject.Singleton
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.*
import js7.base.time.{AdmissionTimeScheme, AlarmClock, TestAlarmClock, Timezone, WeekdayPeriod}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.execution.workflow.instructions.ExecuteExecutor.orderIdToDate
import js7.data.order.Order.Fresh
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.AdmissionTimeSkipJobTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class AdmissionTimeSkipJobTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected def agentPaths = Seq(agentPath)
  protected def items = Seq(singleJobWorkflow, multipleJobsWorkflow)

  private implicit val timeZone = AdmissionTimeSkipJobTest.timeZone
  private val clock = TestAlarmClock(local("2021-09-09T00:00"))

  override protected def controllerModule = new AbstractModule {
    @Provides @Singleton def provideAlarmClock(): AlarmClock = clock
  }

  override protected def agentModule = new AbstractModule {
    @Provides @Singleton def provideAlarmClock(): AlarmClock = clock
  }

  "Skip job if it has no admission time for order date" - {
    "Single job" in {
      val orderId = OrderId("#2021-09-02#single-job")
      assert(orderIdToDate(orderId).map(_.getDayOfWeek) == Some(THURSDAY))

      val events = controller.runOrder(FreshOrder(orderId, singleJobWorkflow.path)).map(_.value)
      assert(events == Seq(
        OrderAdded(singleJobWorkflow.id),
        OrderMoved(Position(1)),
        // Order does not start for skipped order (but for OrderFinished)
        OrderStarted,
        OrderFinished))
    }

    "Between other jobs" in {
      val orderId = OrderId("#2021-09-02#multiple-jobs")
      assert(orderIdToDate(orderId).map(_.getDayOfWeek) == Some(THURSDAY))

      val events = controller.runOrder(FreshOrder(orderId, multipleJobsWorkflow.path)).map(_.value)
      assert(events == Seq(
        OrderAdded(multipleJobsWorkflow.id),

        OrderAttachable(agentPath),
        OrderAttached(agentPath),

        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(3)),  // Positions 1 and 2 are skipped!

        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(4)),

        OrderDetachable,
        OrderDetached,
        OrderFinished))
    }
  }

  "Do not skip if job has a admission time for order date" in {
    clock.resetTo(local("2021-09-10T00:00")) // Friday
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("#2021-09-03#")  // Friday
    assert(orderIdToDate(orderId).map(_.getDayOfWeek) == Some(FRIDAY))

    controllerApi.addOrder(FreshOrder(orderId, singleJobWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
    sleep(100.ms)
    assert(controllerState.idToOrder(orderId).isState[Fresh])

    clock := local("2021-09-10T18:00")
    eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
    eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
  }

  "Do not skip if OrderId has no order date" in {
    clock.resetTo(local("2021-09-10T00:00")) // Friday
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("NO-DATE")
    controllerApi.addOrder(FreshOrder(orderId, singleJobWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
    sleep(100.ms)
    assert(controllerState.idToOrder(orderId).isState[Fresh])

    clock := local("2021-09-10T18:00")
    eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
    eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
  }

  "Do not skip if OrderId has an invalid order date" in {
    clock.resetTo(local("2021-09-10T00:00")) // Friday
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("#2021-02-29#invalid")
    assert(orderIdToDate(orderId).map(_.getDayOfWeek) == None)

    controllerApi.addOrder(FreshOrder(orderId, singleJobWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
    sleep(100.ms)
    assert(controllerState.idToOrder(orderId).isState[Fresh])

    clock := local("2021-09-10T18:00")
    eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
    eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
  }
}

object AdmissionTimeSkipJobTest
{
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val timeZone = ZoneId.of("Europe/Mariehamn")

  private val fridayExecute = Execute(
    WorkflowJob(
      agentPath,
      EmptyJob.executable(),
      admissionTimeScheme = Some(AdmissionTimeScheme(Seq(
        WeekdayPeriod(FRIDAY, LocalTime.of(18, 0), 1.h)))),
      skipIfNoAdmissionForOrderDay = true))

  private val singleJobWorkflow = Workflow(WorkflowPath("SINGLE-JOB") ~ "INITIAL",
    Seq(fridayExecute),
    timeZone = Timezone(timeZone.getId))

  private val multipleJobsWorkflow = Workflow(WorkflowPath("MULTIPLE-JOBS") ~ "INITIAL",
    Seq(
      EmptyJob.execute(agentPath),
      fridayExecute,
      fridayExecute,
      EmptyJob.execute(agentPath)),
    timeZone = Timezone(timeZone.getId))
}
