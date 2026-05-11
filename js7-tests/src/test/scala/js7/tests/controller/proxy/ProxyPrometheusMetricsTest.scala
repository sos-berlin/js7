package js7.tests.controller.proxy

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.lang.management.ManagementFactory
import javax.management.ObjectName
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.common.commandline.CommandLineArguments
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.data.order.OrderEvent.OrderPrompted
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.expression.Expression.expr
import js7.data.workflow.instructions.Prompt
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.proxy.{Proxy, ProxyMainConf}
import js7.tests.controller.proxy.ProxyPrometheusMetricsTest.*
import js7.tests.testenv.ControllerClusterForScalaTest

final class ProxyPrometheusMetricsTest extends OurAsyncTestSuite, ControllerClusterForScalaTest:

  private given IORuntime = ioRuntime
  protected val items = Seq(workflow)

  private lazy val admissionConfig = config"""
    js7.auth.users.Proxy {
      password = "plain:${serverAdmission.userAndPassword.get.password.string}"
      permissions = [ ReadMetrics ]
    }
    """

  override def primaryControllerConfig =
    admissionConfig.withFallback(super.primaryControllerConfig)

  override def backupControllerConfig =
    admissionConfig.withFallback(super.backupControllerConfig)

  private lazy val serverAdmission = controllerAdmissions.head

  "Read /metrics from Proxy, Controller cluster nodes and Subagent" in:
    temporaryDirectoryResource[IO]("ProxyPrometheusMetricsTest-").use: dir =>
      dir / "proxy.conf" :=
        s"""js7.proxy.cluster-nodes = [
           |  {
           |     uri = "${serverAdmission.uri}"
           |     user = Proxy
           |     password = "${serverAdmission.userAndPassword.get.password.string}"
           |  }
           |]""".stripMargin

      val proxyConf = ProxyMainConf.fromCommandLine(CommandLineArguments(List(
        "--cluster-watch-id=MY-CLUSTER-WATCH",  // Run ClusterWatch, too
        s"--config-directory=$dir",
        s"--data-directory=$dir")))

      Proxy.completeResource(proxyConf).use: proxy =>
        IO.blocking:
          runControllerAndBackup(): (_, controller, _, _, _, _, _) =>
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

            proxy.metricsForTest.compile.foldMonoid.map(_.utf8String).await(99.s)
      .map: string =>
        assert(string.contains("""js7_JournaledProxy_EventCount{js7ServerId="Proxy:MY-CLUSTER-WATCH",js7ServerGroupId="Proxy",name="Proxy""""))
        assert(string.contains("""js7_LogDirectory_Size{js7ServerId="Controller/primary",js7ServerGroupId="Engine:Controller""""))
        assert(string.contains("""js7_LogDirectory_Size{js7ServerId="Controller/secondary",js7ServerGroupId="Engine:Controller""""))
        assert(string.contains("""js7_LogDirectory_Size{js7ServerId="Subagent:AGENT-0",js7ServerGroupId="Engine:Controller""""))
        // For bare Subagent, see ControllerPrometheusMetricsTest
        // assert(string.contains("""js7_Sessions_SessionCount{js7ServerId="Subagent:BARE-SUBAGENT"}"""))


object ProxyPrometheusMetricsTest:

  private val workflow = Workflow(WorkflowPath("WORKFLOW"), Seq(
    Prompt(expr"'PROMPT'")))
