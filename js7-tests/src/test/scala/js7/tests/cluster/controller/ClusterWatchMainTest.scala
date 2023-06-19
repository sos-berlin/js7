package js7.tests.cluster.controller

import cats.effect.{ContextShift, IO}
import cats.effect.concurrent.Deferred
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.cluster.watch.ClusterWatchMain
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.cluster.controller.ClusterWatchMainTest.*
import js7.tests.testenv.ControllerClusterForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced as scheduler

final class ClusterWatchMainTest extends OurAsyncTestSuite with ControllerClusterForScalaTest
{
  private implicit lazy val cs: ContextShift[IO] = IO.contextShift(scheduler)
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

      val clusterWatch = IO(
        try
          withTemporaryDirectory("ClusterWatchMainTest-") { dir =>
            dir / "cluster-watch.conf" :=
              s"""js7.journal.cluster.watch.cluster-nodes = [
                |  {
                |     uri = "${serverAdmission.uri}"
                |     user = ClusterWatch
                |     password = "${serverAdmission.userAndPassword.get.password.string}"
                |  }
                |]""".stripMargin

            val args = Array(
              "--cluster-watch-id=MY-CLUSTER-WATCH",
              "--config-directory=" + dir)
            ClusterWatchMain.run(args)(service => Task
              .race(
                service.untilStopped,
                stopClusterWatch.get.to[Task])
              .as(ProgramTermination()))
          }
        catch { case t: Throwable =>
          logger.error(t.toStringWithCauses, t)
        })

      val runAnOrder = IO {
        runControllerAndBackup(suppressClusterWatch = true) { (_, controller, _, _, _, _, _) =>
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
      } yield succeed
    }.unsafeToFuture()
  }
}

object ClusterWatchMainTest
{
  private val logger = Logger[this.type]
  private val workflow = Workflow(WorkflowPath("WORKFLOW"), Nil)
}
