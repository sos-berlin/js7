package js7.tests.controller.cluster

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked.Ops
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost}
import js7.data.controller.ControllerCommand.ShutDown
import monix.execution.Scheduler.Implicits.global

final class ManualPassiveLostClusterTest extends ControllerClusterTester
{
  override protected def primaryControllerConfig =
    // Short timeout because something blocks web server shutdown occasionally
    config"""js7.web.server.shutdown-timeout = 0.5s"""
      .withFallback(super.primaryControllerConfig)

  "PassiveLost" in {
    withControllerAndBackup(noClusterWatch = true) { (primary, backup, clusterSetting) =>
      val backupId = clusterSetting.passiveId
      val primaryController = primary.startController(httpPort = Some(primaryControllerPort)) await 99.s
      val backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s
      primaryController.eventWatch.await[ClusterCoupled]()

      // KILL BACKUP
      backupController.executeCommandAsSystemUser(ShutDown())
        .await(99.s).orThrow
      backupController.terminated await 99.s

      val ClusterPassiveLost(`backupId`) = primaryController.eventWatch.await[ClusterPassiveLost]()
        .head.value.event

      primaryController.terminate() await 99.s
    }
  }
}
