package js7.tests.plan

import js7.base.configutils.Configs.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TestAlarmClock
import js7.base.time.TimestampForTests.ts
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{ChangePlan, ChangePlanSchema}
import js7.data.plan.PlanEvent.PlanDeleted
import js7.data.plan.PlanStatus.{Closed, Deleted, Finished}
import js7.data.plan.{Plan, PlanSchema, PlanSchemaId}
import js7.data.value.Value.convenience.given
import js7.tests.plan.PlanDeletedTest.*
import js7.tests.testenv.{ControllerAgentForScalaTest, DirectoryProvider}
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

final class PlanDeletedTest
  extends OurTestSuite, ControllerAgentForScalaTest:

  private val startTime = ts"2025-03-12T00:00:00Z"
  private lazy val clock = TestAlarmClock(startTime)

  override protected def controllerTestWiring = RunningController.TestWiring(
    alarmClock = Some(clock))

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  "Engine deletes finished plan immediately if finishedPlanRetentionPeriod = 0" in:
    withItem(dailyPlan): dailyPlan =>
      eventWatch.resetLastWatchedEventId()

      val tomorrow = "2025-03-12"
      val planId = dailyPlan.id / "2025-03-11"

      execCmd:
        ChangePlan(planId, Closed)
      clock.tick()

      assert(controllerState.toPlan.values.toSeq == Seq(
        Plan(planId, Deleted)))

      assert(controllerState.toPlan.toMap == Map(planId -> Plan(planId, Deleted)))

      execCmd:
        ChangePlanSchema(dailyPlan.id, Some(Map("unknownPlansAreOpenFrom" -> tomorrow)))

      // Deleted Plan as been removed
      assert(controllerState.toPlan.isEmpty)

  "Engine deletes finished plan after finishedPlanRetentionPeriod" in:
    withItem(dailyPlan): dailyPlan =>
      eventWatch.resetLastWatchedEventId()
      execCmd:
        ChangePlanSchema(dailyPlan.id, finishedPlanRetentionPeriod = Some(1.h))

      val planId = dailyPlan.id / "2025-03-11"

      execCmd:
        ChangePlan(planId, Closed)
      clock.tick()
      assert(controllerState.toPlan.values.toSeq == Seq(
        Plan(planId, Finished(startTime))))

      clock.tick(1.h - 1.s)
      sleep(100.ms)
      assert(controllerState.toPlan.values.toSeq == Seq(
        Plan(planId, Finished(startTime))))

      clock.tick(1.s)
      eventWatch.awaitNextKey[PlanDeleted](planId)
      assert:
        controllerState.toPlan.values.toSeq == Seq(Plan(planId, Deleted))

  "ChangePlanSchema(finishedPlanRetentionPeriod) changes PlanDeleted timers" in:
    withItem(dailyPlan): dailyPlan =>
      clock.resetTo(startTime)
      eventWatch.resetLastWatchedEventId()
      execCmd:
        ChangePlanSchema(dailyPlan.id, finishedPlanRetentionPeriod = Some(1.h))

      val planId = dailyPlan.id / "2025-03-11"

      execCmd:
        ChangePlan(planId, Closed)
      clock.tick()
      assert(controllerState.toPlan.values.toSeq == Seq(
        Plan(planId, Finished(startTime))))

      clock.tick(1.minute)
      execCmd:
        ChangePlanSchema(dailyPlan.id, finishedPlanRetentionPeriod = Some(1.minute))
      clock.tick() // PlanDeleted events are scheduled

      eventWatch.awaitNextKey[PlanDeleted](planId)
      assert:
        controllerState.toPlan.values.toSeq == Seq(Plan(planId, Deleted))


  "Finished Plans are deleted after Controller restart, too" in:
    clock.resetTo(startTime)
    val directoryProvider = DirectoryProvider(
      agentPaths = Nil,
      controllerTestWiring = controllerTestWiring,
      items = Seq(dailyPlan))

    val planId = dailyPlan.id / "2025-03-11"

    directoryProvider.runController(): controller =>
      controller.execCmd:
        ChangePlanSchema(dailyPlan.id, finishedPlanRetentionPeriod = Some(1.h))
      controller.execCmd:
        ChangePlan(planId, Closed)
      clock.tick()
      assert(controller.controllerState().toPlan.values.toSeq == Seq(
        Plan(planId, Finished(startTime))))

    directoryProvider.runController(): controller =>
      assert(controller.controllerState().toPlan.values.toSeq == Seq(
        Plan(planId, Finished(startTime))))

      clock.tick(1.h)
      controller.eventWatch.awaitKey[PlanDeleted](planId)
      assert:
        controller.controllerState().toPlan.values.toSeq == Seq(Plan(planId, Deleted))


object PlanDeletedTest:
  private val agentPath = AgentPath("AGENT")
  private val dailyPlan = PlanSchema.joc(PlanSchemaId("DailyPlan"))
