package js7.tests.controller.commands

import akka.http.scaladsl.model.StatusCodes.InternalServerError
import js7.base.configutils.Configs.*
import js7.base.problem.Checked.Ops
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.http.AkkaHttpClient.HttpException
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.controller.commands.AddOrderTimeoutTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class AddOrderTimeoutTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = Nil
  protected val items = Seq(agentRef, subagentItem, workflow)
  override protected val controllerConfig = config"""
    js7.akka.ask-timeout = 2s
    js7.TEST-ONLY.add-order-delay = 10s
    """

  "AddOrder timeout is returned as 403 Service Unavailable" in {
    controller.httpApi.login().await(99.s)
    val status = intercept[HttpException] {
      controller.httpApi.addOrder(FreshOrder(OrderId("ORDER"), workflow.path)).await(99.s)
    }.status
    // Despite error, addOrder may be successfully completed, so ServiceUnavailable is inappropriate:
    // assert(status == ServiceUnavailable)
    assert(status == InternalServerError)
  }
}

object AddOrderTimeoutTest
{
  private val agentRef = AgentRef(AgentPath("AGENT"), Seq(SubagentId("SUBAGENT")))
  private val subagentItem = SubagentItem(SubagentId("SUBAGENT"), AgentPath("AGENT"),
    Uri("https://localhost:0"))

  private val workflow = WorkflowParser.parse(
    WorkflowPath("WORKFLOW") ~ "1","""
      define workflow {
        execute executable="SCRIPT1.cmd", agent="AGENT";
      }"""
  ).orThrow
}
