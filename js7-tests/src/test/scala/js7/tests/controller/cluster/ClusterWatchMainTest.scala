package js7.tests.controller.cluster

import cats.effect.IO
import cats.effect.concurrent.Deferred
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.test.OurTestSuite
import js7.cluster.watch.{ClusterWatchConf, ClusterWatchMain}
import js7.common.commandline.CommandLineArguments
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.cluster.ClusterWatchMainTest.*
import js7.tests.testenv.ControllerClusterForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced as scheduler

final class ClusterWatchMainTest extends OurTestSuite with ControllerClusterForScalaTest
{
  private implicit lazy val cs = IO.contextShift(scheduler)
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

  "test" in {
    IO.defer {
      val stopClusterWatch = Deferred.unsafe[IO, Unit]

      val clusterWatch = IO {
        withTemporaryDirectory("ClusterWatchMainTest-") { dir =>
          dir / "cluster-watch.conf" :=
            s"""js7.cluster.watch.cluster-nodes = [
              |  {
              |     uri = "${serverAdmission.uri}"
              |     user = ClusterWatch
              |     password = "${serverAdmission.userAndPassword.get.password.string}"
              |  }
              |]""".stripMargin

          val conf = ClusterWatchConf.fromCommandLine(CommandLineArguments(Seq(
            "--config-directory=" + dir)))
          ClusterWatchMain.run(conf)(service => Task
            .race(
              service.untilStopped,
              Task.from(stopClusterWatch.get))
            .void)
        }
      }

      val runAnOrder = IO {
        runControllerAndBackup(suppressClusterWatch = true) { (_, controller, _, _, _) =>
          controller.eventWatch.await[ClusterCoupled]()
          controller.runOrder(FreshOrder(OrderId("ORDER"), workflow.path))
          ()
        }
      }

      for {
        runAnOrder <- runAnOrder.start
        clusterWatch <- clusterWatch.start
        _ <- runAnOrder.join
        _ <- stopClusterWatch.complete(())
        _ <- clusterWatch.join
      } yield ()
    }.unsafeRunSync()
  }
}

object ClusterWatchMainTest
{
  private val workflow = Workflow(WorkflowPath("WORKFLOW"), Nil)
}
