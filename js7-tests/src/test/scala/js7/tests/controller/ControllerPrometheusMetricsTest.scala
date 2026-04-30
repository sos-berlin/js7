package js7.tests.controller

import cats.effect.IO
import java.lang.management.ManagementFactory
import javax.management.ObjectName
import js7.base.catsutils.CatsEffectExtensions.orThrow
import js7.base.circeutils.CirceUtils.*
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.OrderPrompted
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.value.expression.Expression.expr
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.Prompt
import js7.tests.controller.ControllerPrometheusMetricsTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId

final class ControllerPrometheusMetricsTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem, ReadMetrics ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = true
    js7.auth.subagents.BARE-SUBAGENT = "${toLocalSubagentId(agentPath)}'s PASSWORD"
    """.withFallback(super.agentConfig)

  protected val bareSubagentId = SubagentId("BARE-SUBAGENT")
  protected lazy val bareSubagentItem = SubagentItem(
    bareSubagentId,
    agentPath,
    findFreeLocalUri())

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(bareSubagentItem)

  "Via PlatformMBeanServer" in:
    val workflow = Workflow.of:
      Prompt(expr"'PROMPT'")

    withItem(workflow): workflow =>
      val orderId = OrderId("ORDER")
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
      controller.eventWatch.awaitNextKey[OrderPrompted](orderId)
      val beanServer = ManagementFactory.getPlatformMBeanServer

      // Because tests run in parallel, more than one of EngineState MXBean may be registered.
      // We don't know which is ours. But there must be at least one.
      val objectNames = beanServer.queryNames(new ObjectName("js7:type=EngineState,*"), null)
      assert(!objectNames.isEmpty)

      //assert:
      //  beanServer.getAttribute(new ObjectName("js7:type=EngineState,*"), "EventTotal").asInstanceOf[Long] > 2

  "Via /metrics web service" in:
    runSubagent(bareSubagentItem, suppressSignatureKeys = true): _ =>
      val lines: Seq[String] =
        fs2.Stream.force:
          controller.api.httpGetRawLinesStream("/metrics").orThrow
        .map(_.utf8String).compile.toVector.await(99.s)

      lines.foreachWithBracket()((line, br) => logger.info(s"$br${line.trim}"))

      assert(lines.exists(_.startsWith:
        """js7_EngineState_OrderCount{js7Server="Controller/primary"}"""))
      assert(lines.exists(_.startsWith:
        """js7_EngineState_OrderCount{js7Server="Subagent:AGENT-0"}"""))
      assert(lines.exists(_.startsWith:
        """js7_EngineState_OrderCount{js7Server="Subagent:BARE-SUBAGENT"}"""))
      assert(lines.exists(_.startsWith:
        """js7_EngineState_OrderCount{js7Server="Controller/primary"}"""))
      assert(lines.exists(_.startsWith:
        """js7_EngineState_OrderCount{js7Server="Subagent:AGENT-0"}"""))
      assert(lines.exists(_.startsWith:
        """js7_EngineState_OrderCount{js7Server="Subagent:BARE-SUBAGENT"}"""))

  "/grafana/dashboard" in:
    val json = controller.api.httpGetJson("/grafana/dashboard")
      .orThrow.await(99.s).parseJson.orThrow
    assert(json.isObject)


object ControllerPrometheusMetricsTest:
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
