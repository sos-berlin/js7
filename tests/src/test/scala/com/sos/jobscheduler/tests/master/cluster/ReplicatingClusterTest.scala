package com.sos.jobscheduler.tests.master.cluster

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import com.sos.jobscheduler.data.cluster.ClusterEvent
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.events.MasterEvent.MasterReady
import com.sos.jobscheduler.tests.master.cluster.MasterClusterTester._
import monix.execution.Scheduler.Implicits.global

final class ReplicatingClusterTest extends MasterClusterTester
{
  "Cluster replicates journal files properly" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      val primaryMaster = primary.startMaster(httpPort = Some(primaryHttpPort)) await 99.s
      primaryMaster.eventWatch.await[MasterReady]()
      primaryMaster.runOrder(FreshOrder(OrderId("🔶"), TestWorkflow.path))

      val backupMaster = backup.startMaster(httpPort = Some(backupHttpPort)) await 99.s
      primaryMaster.eventWatch.await[ClusterEvent.FollowingStarted]()

      //assert(!primaryMaster.journalActorState.isRequiringClusterAcknowledgement)
      //primaryMaster.executeCommandAsSystemUser(
      //  ClusterAppointBackup(Uri(backupMaster.localUri.toString), Uri(primaryMaster.localUri.toString))
      //).await(99.s).orThrow
      //primaryMaster.eventWatch.await[ClusterEvent.BackupNodeAppointed]()
      primaryMaster.eventWatch.await[ClusterEvent.ClusterCoupled]()
      assert(primaryMaster.journalActorState.isRequiringClusterAcknowledgement)

      primaryMaster.runOrder(FreshOrder(OrderId("🔷"), TestWorkflow.path))

      assertEqualJournalFiles(primary.master, backup.master, 1)

      primaryMaster.executeCommandAsSystemUser(MasterCommand.TakeSnapshot).await(99.s).orThrow
      assertEqualJournalFiles(primary.master, backup.master, 2)

      primaryMaster.runOrder(FreshOrder(OrderId("🔵"), TestWorkflow.path))
      assertEqualJournalFiles(primary.master, backup.master, 2)

      primaryMaster.terminate() await 99.s
      backupMaster.terminate() await 99.s
      assertEqualJournalFiles(primary.master, backup.master, 3)  // TODO Maybe "ShutDown" has not been replicated

      // RESTART

      backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupMaster =>
        primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
          assert(primaryMaster.journalActorState.isRequiringClusterAcknowledgement)
          val lastEventId = primaryMaster.eventWatch.lastAddedEventId
          primaryMaster.runOrder(FreshOrder(OrderId("🔹"), TestWorkflow.path))
          primaryMaster.eventWatch.await[OrderFinished](_.key == OrderId("🔹"), after = lastEventId)
          backupMaster.eventWatch.await[OrderFinished](_.key == OrderId("🔹"), after = lastEventId)

          // Check acknowledgement of empty event list
          primaryMaster.httpApi.login(Some(UserId("TEST") -> SecretString("TEST-PASSWORD"))).await(99.s)
          primaryMaster.httpApi.addOrders(Nil).await(99.s)  // Issues no events
        }
      }
    }
  }
}
