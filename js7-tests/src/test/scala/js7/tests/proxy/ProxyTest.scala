package js7.tests.proxy

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, ExitCode, IO}
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.ProgramTermination
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.proxy.Proxy
import js7.tests.proxy.ProxyTest.*
import js7.tests.testenv.ControllerClusterForScalaTest

final class ProxyTest extends OurAsyncTestSuite, ControllerClusterForScalaTest:

  private given IORuntime = ioRuntime
  protected val items = Seq(workflow)

  private lazy val admissionConfig = config"""
    js7.auth.users.Proxy {
      password = "plain:${serverAdmission.userAndPassword.get.password.string}"
    }
    """

  override def primaryControllerConfig =
    admissionConfig.withFallback(super.primaryControllerConfig)

  override def backupControllerConfig =
    admissionConfig.withFallback(super.backupControllerConfig)

  private lazy val serverAdmission = controllerAdmissions.head

  "Proxy" in :
    val stopProxy = Deferred.unsafe[IO, Unit]

    val proxy =
      temporaryDirectoryResource[IO]("ProxyTest-").use: dir =>
        IO.defer:
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
            "--config-directory=" + dir)

          Proxy.runAsTest(args): service =>
            IO.race(stopProxy.get, service.untilStopped)
              .as(ProgramTermination())

    val runAnOrder = IO:
      runControllerAndBackup(suppressClusterWatch = true): (_, controller, _, _, _, _, _) =>
        controller.eventWatch.await[ClusterCoupled]()
        controller.runOrder(FreshOrder(OrderId("ORDER"), workflow.path))
        ()

    for
      runAnOrder <- runAnOrder.start
      clusterWatch <- proxy.start
      _ <- runAnOrder.joinStd
      _ <- stopProxy.complete(())
      exitCode <- clusterWatch.joinStd
    yield
      assert(exitCode == ExitCode.Success)


object ProxyTest:
  private val workflow = Workflow(WorkflowPath("WORKFLOW"), Nil)
