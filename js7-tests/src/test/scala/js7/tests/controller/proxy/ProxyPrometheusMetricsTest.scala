package js7.tests.controller.proxy

import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO}
import java.lang.management.ManagementFactory
import javax.management.ObjectName
import js7.base.catsutils.CatsEffectExtensions.orThrow
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ProgramTermination
import js7.data.order.OrderEvent.OrderPrompted
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.expression.Expression.expr
import js7.data.workflow.instructions.Prompt
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.proxy.Proxy
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
      IO.blocking:
        dir / "proxy.conf" :=
          s"""js7.proxy.cluster-nodes = [
             |  {
             |     uri = "${serverAdmission.uri}"
             |     user = Proxy
             |     password = "${serverAdmission.userAndPassword.get.password.string}"
             |  }
             |]""".stripMargin

        val args = List(
          "--cluster-watch-id=MY-CLUSTER-WATCH",  // Run ClusterWatch, too
          s"--config-directory=$dir",
          s"--data-directory=$dir")

        val termination =
          Proxy.runAsTest(args): proxy =>
            IO.blocking:
              runControllerAndBackup(): (_, controller, _, _, _, _, _) =>
                val orderId = OrderId("ORDER")
                controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
                controller.eventWatch.awaitNextKey[OrderPrompted](orderId)
                val beanServer = ManagementFactory.getPlatformMBeanServer

                // Because tests run in parallel, more than one of EngineState MXBean may be registered.
                // We don't know which is ours. But there must be at least one.
                val objectNames = beanServer.queryNames(new ObjectName("js7:name=EngineState,*"), null)
                assert(!objectNames.isEmpty)
                //assert:
                //  beanServer.getAttribute(new ObjectName("js7:name=EngineState,*"), "EventTotal").asInstanceOf[Long] > 2

                val string = proxy.metrics.orThrow.flatMap(_.compile.foldMonoid).map(_.utf8String)
                  .await(99.s)
                assert(string.contains("""js7_Sessions_SessionCount{js7Server="Proxy:MY-CLUSTER-WATCH"}"""))
                assert(string.contains("""js7_Sessions_SessionCount{js7Server="Controller/primary"}"""))
                assert(string.contains("""js7_Sessions_SessionCount{js7Server="Controller/backup"}"""))
                assert(string.contains("""js7_Sessions_SessionCount{js7Server="Subagent:AGENT-0"}"""))
                // For bare Subagent, see ControllerPrometheusMetricsTest
                // assert(string.contains("""js7_Sessions_SessionCount{js7Server="Subagent:BARE-SUBAGENT"}"""))
              ProgramTermination()
          .await(99.s)
        assert(termination == ExitCode.Success) // On failure, see log for error


object ProxyPrometheusMetricsTest:

  private val workflow = Workflow(WorkflowPath("WORKFLOW"), Seq(
    Prompt(expr"'PROMPT'")))
