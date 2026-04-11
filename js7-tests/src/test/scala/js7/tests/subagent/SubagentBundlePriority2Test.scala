package js7.tests.subagent

import cats.effect.IO
import cats.effect.kernel.Resource
import js7.agent.RunningAgent
import js7.agent.motor.JobMotor
import js7.base.catsutils.Environment.TaggedResource
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.eventbus.{EventPublisher, StandardEventBus}
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.subagent.{SubagentBundle, SubagentBundleId}
import js7.data.value.expression.Expression.convenience.given
import js7.data.value.expression.Expression.expr
import js7.data.workflow.instructions.{Execute, Fork}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.subagent.jobs.TestJob
import js7.tests.subagent.SubagentBundlePriority2Test.*
import js7.tests.subagent.SubagentTester.agentPath
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions

final class SubagentBundlePriority2Test extends OurTestSuite, ControllerAgentForScalaTest:

  private given ExecutionContext = ioRuntime.compute

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = true
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  private lazy val eventBus = StandardEventBus[JobMotor.TestOrderConcurrentlyChanged]()

  override protected def agentTestWiring = RunningAgent.TestWiring(
    envResources = Seq(TaggedResource(Resource.eval(IO:
      eventBus: EventPublisher[JobMotor.TestOrderConcurrentlyChanged]))))

  "Warning 'Order has been removed concurrently'" in:
    // Test is slow because SubagentKeeper polls for an available Subagent, half a second per round
    val n = if isIntelliJIdea then 100 else 5

    def executeJob(sleep: FiniteDuration = 0.s): Execute.Anonymous =
      TestJob.execute(agentPath, Map("sleep" -> sleep.toBigDecimalSeconds),
        subagentBundleId = Some("BUNDLE"), processLimit = 100)

    val workflow = Workflow(WorkflowPath("WORKFLOW"), Seq(
      Fork.of(
        "1" -> Workflow.of(
          executeJob(),
          executeJob()),
        "2" -> Workflow.of(
          executeJob(10.ms)),
        "3" -> Workflow.of(
          executeJob(10.ms)))))

    val subagentBundle = SubagentBundle(
      SubagentBundleId("BUNDLE"),
      subagentToPriority = Map(
        localSubagentId ->
          // Number of processes started via our subagent bundle, allow 1
          expr"if $$js7ClusterSubagentProcessCount < 1 then 1 else missing"))

    var orderConcurrentlyChanged = false
    eventBus.subscribe: evt =>
      Logger.error(s"$evt")
      orderConcurrentlyChanged = true

    withItems((subagentBundle, workflow)): _ =>
      (1 to n).foreach: i =>
        logger.info("•" * 100 + i)
        runOrder(nextOrderId(), workflow.path)
        assert(!orderConcurrentlyChanged)


object SubagentBundlePriority2Test:
  private val logger = Logger[this.type]
  private val localSubagentId = toLocalSubagentId(agentPath)
