package js7.tests.controller.commands

import js7.base.auth.{UserAndPassword, UserId}
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.http.PekkoHttpClient.HttpException
import js7.controller.client.{HttpControllerApi, PekkoHttpControllerApi}
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.controller.commands.AddOrderTimeoutTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import org.apache.pekko.http.scaladsl.model.StatusCodes.InternalServerError

final class AddOrderTimeoutTest extends OurTestSuite, ControllerAgentForScalaTest:

  protected val agentPaths = Nil
  protected val items = Seq(agentRef, subagentItem, workflow)
  override protected val controllerConfig = config"""
    js7.pekko.ask-timeout = 2s
    js7.TEST-ONLY.add-order-delay = 10s
    """

  "AddOrder timeout is returned as 403 Service Unavailable" in:
    val httpApi: HttpControllerApi =
      new PekkoHttpControllerApi(
        controller.localUri,
        Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD"))),
          actorSystem = controller.actorSystem, config = controller.config, name = controller.conf.name)

    httpApi.login().await(99.s)
    val status = intercept[HttpException] {
      httpApi.addOrder(FreshOrder(OrderId("ORDER"), workflow.path)).await(99.s)
    }.status
    // Despite error, addOrder may be successfully completed, so ServiceUnavailable is inappropriate:
    // assert(status == ServiceUnavailable)
    assert(status == InternalServerError)


object AddOrderTimeoutTest:
  private val agentRef = AgentRef(AgentPath("AGENT"), Seq(SubagentId("SUBAGENT")))
  private val subagentItem = SubagentItem(SubagentId("SUBAGENT"), AgentPath("AGENT"),
    Uri("https://localhost:0"))

  private val workflow = WorkflowParser.parse(
    WorkflowPath("WORKFLOW") ~ "1","""
      define workflow {
        execute executable="SCRIPT1.cmd", agent="AGENT";
      }"""
  ).orThrow
