package js7.tests.addOrders

import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.JavaResource
import js7.base.log.{CorrelId, CorrelIdLog4jThreadContextMap, Logger}
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.durationAndPerSecondString
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.OrderDeleted
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.addOrders.TestAddOrdersTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.duration.FiniteDuration

final class TestAddOrdersTest extends OurTestSuite, ControllerAgentForScalaTest:

  protected val agentPaths = Seq(AgentPath("agent-1"), AgentPath("agent-2"))
  protected val items = Seq(workflow)

  override def afterAll(): Unit =
    super.afterAll()
    CorrelId.logStatistics()

  override protected def controllerConfig = config"""
    js7.auth.users.TestAddOrders.password = "plain:TEST-PASSWORD"
    """ withFallback super.controllerConfig

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """ withFallback super.agentConfig

  "TestAddOrders" in:
    val orderCount = sys.props.get("test.speed").map(_.toInt) getOrElse 3

    def logOrderCountChanged(statistics: Statistics) =
      if statistics.totalOrderCount > 0 then
        logger.info(s"${statistics.lastOrderCount} orders")

    def logAddOrdersDuration(duration: FiniteDuration) =
      info(durationAndPerSecondString(duration, orderCount, "orders added"))

    val settings = Settings.parseArguments(Seq(
      "--controller=" + controller.localUri,
      "--workflow=" + workflow.path.string,
      "--count=" + orderCount,
      "--user=TestAddOrders:TEST-PASSWORD"))
    val statistics = TestAddOrders.run(settings, logOrderCountChanged, logAddOrdersDuration)
      .await(99.s).orThrow
    controller.eventWatch.await[OrderDeleted](_.key.string startsWith "TestAddOrders-")
    for line <- statistics.logLines do info(line)
    CorrelId.logStatisticsIfEnabled()
    CorrelIdLog4jThreadContextMap.logStatistics()

private object TestAddOrdersTest:
  private val logger = Logger[this.type]

  private val workflow =
    JavaResource(
      "js7/install/docker/volumes/provider/config/live/testCase3Empty.workflow.json"
    ).asUTF8String.parseJsonAs[Workflow].orThrow
      .withId(WorkflowPath("testCase3"))
