package js7.tests.cluster.controller

import java.io.RandomAccessFile
import js7.base.io.file.FileUtils.syntax.*
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterPassiveLost}
import js7.data.cluster.ClusterState.Coupled
import js7.data.order.{FreshOrder, OrderId}
import js7.journal.files.JournalFiles.listJournalFiles
import js7.tests.cluster.controller.ControllerClusterTester.*
import js7.tests.testenv.ControllerEnv

final class TruncatedJournalFileControllerClusterTest extends ControllerClusterTester:

  override protected def removeObsoleteJournalFiles = false

  "Backup node replicates truncated journal file" in:
    withControllerAndBackup(): (primary, _, backup, _, _) =>
      primary.runController(): primaryController =>
        val backupController = backup.newController()

        backupController.eventWatch.awaitNext[ClusterCoupled]()
        backupController.stop.await(99.s)
        primaryController.eventWatch.awaitNext[ClusterPassiveLost]()

        primaryController.terminate(suppressSnapshot = true).await(99.s)

      truncateLastJournalFile(primary.controllerEnv)

      primary.runController(dontWaitUntilReady = true/*Since v2.7*/): primaryController =>
        backup.runController(dontWaitUntilReady = true): _ =>
          primaryController.waitUntilReady() // Since v2.7
          primaryController.eventWatch.awaitNext[ClusterCoupled](
            after = primaryController.eventWatch.lastFileEventId)
          //assertEqualJournalFiles(primary.controller, backup.controller, n = 2)
          primaryController.runOrder(FreshOrder(OrderId("🔷"), TestWorkflow.path))

          if !primaryController.clusterState.await(99.s).isInstanceOf[Coupled] then
            // On a busy machine (quite often when tests run), the passive node may send a
            // ClusterRecouple lately, and a second ClusterPassiveLost is emitted.
            // This is due to a race condition with the primary's ClusterPassiveLost
            primaryController.eventWatch.awaitNext[ClusterCoupled]()

          assert(primaryController.clusterState.await(99.s).isInstanceOf[Coupled])
          primaryController.terminate().await(99.s)

  private def truncateLastJournalFile(env: ControllerEnv): Unit =
    val lastFile = listJournalFiles(env.stateDir / "controller").last.file
    assert(lastFile.contentString.last == '\n')
    autoClosing(new RandomAccessFile(lastFile.toFile, "rw")): f =>
      f.setLength(f.length - 2)
    assert(lastFile.contentString.last != '\n')
