package js7.tests

import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.perSecondString
import js7.common.configutils.Configs.HoconStringInterpolator
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.JavaResource
import js7.data.agent.AgentId
import js7.data.order.OrderEvent.OrderRemoved
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.TestAddOrdersTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.FiniteDuration

final class TestAddOrdersTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = Seq(AgentId("agent-1"), AgentId("agent-2"))
  protected val versionedItems = Seq(workflow)

  override protected def controllerConfig = config"""
    js7.auth.users.TestAddOrders.password = "plain:TestAddOrders"
    """ withFallback super.controllerConfig

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """ withFallback super.agentConfig

  "TestAddOrders" in {
    val n = sys.props.get("test.speed").map(_.toInt) getOrElse 3

    def logAddOrdersDuration(duration: FiniteDuration) = Task {
      info(perSecondString(duration, n, "orders added"))
    }

    val settings = TestAddOrders.parseArguments(Seq(
      "--controller=" + controller.localUri,
      "--workflow=" + workflow.path.string,
      "--count=" + n,
      "--user=TestAddOrders",
      "--password=TestAddOrders"))
    val statistics = TestAddOrders.run(settings, logAddOrdersDuration).await(99.s).orThrow
    controller.eventWatch.await[OrderRemoved](_.key.string startsWith "TestAddOrders-")
    for (line <- statistics.logLines) info(line)
  }
}

private object TestAddOrdersTest
{
  private val workflow =
    JavaResource(
      "js7/install/docker/volumes/provider/config/live/testCase3.workflow.json"
    ).asUTF8String.parseJsonCheckedAs[Workflow].orThrow
      .withId(WorkflowPath("/testCase3"))
}
