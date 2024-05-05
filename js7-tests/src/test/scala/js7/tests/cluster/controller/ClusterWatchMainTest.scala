package js7.tests.cluster.controller

import cats.effect.{Deferred, IO}
import cats.effect.unsafe.IORuntime
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.ProgramTermination
import js7.cluster.watch.ClusterWatchMain
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.cluster.controller.ClusterWatchMainTest.*
import js7.tests.testenv.ControllerClusterForScalaTest
import js7.base.catsutils.CatsEffectExtensions.joinStd

final class ClusterWatchMainTest extends OurAsyncTestSuite, ControllerClusterForScalaTest:

  private given IORuntime = ioRuntime

  private lazy val serverAdmission = controllerAdmissions.head

  private lazy val admissionConfig = config"""
    js7.auth.users.ClusterWatch {
      password = "plain:${serverAdmission.userAndPassword.get.password.string}"
    }
    """

  override def primaryControllerConfig =
    admissionConfig.withFallback(super.primaryControllerConfig)

  override def backupControllerConfig =
    admissionConfig.withFallback(super.backupControllerConfig)

  protected val items = Seq(workflow)

  "test" in:
    val stopClusterWatch = Deferred.unsafe[IO, Unit]

    val clusterWatch =
      temporaryDirectoryResource[IO]("ClusterWatchMainTest-").use: dir =>
        IO.defer:
          dir / "cluster-watch.conf" :=
            s"""js7.journal.cluster.watch.cluster-nodes = [
              |  {
              |     uri = "${serverAdmission.uri}"
              |     user = ClusterWatch
              |     password = "${serverAdmission.userAndPassword.get.password.string}"
              |  }
              |]""".stripMargin

          val args = List(
            "--cluster-watch-id=MY-CLUSTER-WATCH",
            "--config-directory=" + dir)

          ClusterWatchMain.runAsTest(args): service =>
            IO.race(
                service.untilStopped,
                stopClusterWatch.get.to[IO])
              .as(ProgramTermination())

    val runAnOrder = IO:
      runControllerAndBackup(suppressClusterWatch = true): (_, controller, _, _, _, _, _) =>
        controller.eventWatch.await[ClusterCoupled]()
        controller.runOrder(FreshOrder(OrderId("ORDER"), workflow.path))
        ()

    for
      runAnOrder <- runAnOrder.start
      clusterWatch <- clusterWatch.start
      _ <- runAnOrder.joinStd
      _ <- stopClusterWatch.complete(())
      _ <- clusterWatch.joinStd
    yield succeed


object ClusterWatchMainTest:
  private val workflow = Workflow(WorkflowPath("WORKFLOW"), Nil)
