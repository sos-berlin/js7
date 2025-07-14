package js7.tests

import cats.effect.IO
import java.lang.management.ManagementFactory
import js7.base.catsutils.CatsEffectExtensions.orThrow
import js7.base.configutils.Configs.*
import js7.base.log.Logger
import js7.base.system.MBeanUtils.toMBeanName
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.OrderPrompted
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.value.expression.Expression.expr
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.Prompt
import js7.tests.testenv.ControllerAgentForScalaTest

final class PrometheusMetricsTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem, ReadMetrics ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(AgentPath("AGENT"))
  protected val items = Nil

  "Via PlatformMBeanServer" in:
    val workflow = Workflow.of:
      Prompt(expr"'PROMPT'")

    withItem(workflow): workflow =>
      val orderId = OrderId("ORDER")
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
      controller.eventWatch.awaitNextKey[OrderPrompted](orderId)
      val beanServer = ManagementFactory.getPlatformMBeanServer
      assert:
        beanServer.getAttribute(toMBeanName("EngineState"), "EventTotal").asInstanceOf[Long] > 2

  "Via /metrics web service" in:
      val lines = controller.api.httpGetRawLinesStream("/metrics")
        .orThrow
        .flatMap:
          _.map(_.utf8String)
          .compile.toVector
        .await(99.s)
      assert(lines.exists(_.startsWith("js7_EngineState_OrderCount ")))

  "/grafana/dashboard" in:
    val lines = controller.api.httpGetRawLinesStream("/grafana/dashboard")
      .orThrow
      .flatMap:
        _.map(_.utf8String)
          .compile.toVector
      .await(99.s)
    assert(lines.head.startsWith("{"))


object PrometheusMetricsTest:
  private val logger = Logger[this.type]
