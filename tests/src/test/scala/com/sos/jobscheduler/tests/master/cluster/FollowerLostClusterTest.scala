package com.sos.jobscheduler.tests.master.cluster

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import com.sos.jobscheduler.data.cluster.ClusterEvent
import com.sos.jobscheduler.data.cluster.ClusterEvent.{Coupled, FollowerLost}
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.tests.master.cluster.MasterClusterTester._
import monix.execution.Scheduler.Implicits.global

final class FollowerLostClusterTest extends MasterClusterTester
{
  "Passive lost" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      val primaryMaster = primary.startMaster(httpPort = Some(primaryHttpPort)) await 99.s
      var backupMaster = backup.startMaster(httpPort = Some(backupHttpPort)) await 99.s

      //primaryMaster.executeCommandAsSystemUser(
      //  ClusterAppointBackup(activeUri = Uri(primaryMaster.localUri.toString), backupUri = Uri(backupMaster.localUri.toString))
      //).await(99.s).orThrow
      primaryMaster.eventWatch.await[ClusterEvent.Coupled]()

      var orderId = OrderId("🔺")
      primaryMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
      primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)
      backupMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)

      // KILL BACKUP
      backupMaster.terminate() await 99.s
      val followerLost = primaryMaster.eventWatch.await[FollowerLost](_.key == NoKey).head.eventId

      primaryMaster.eventWatch.await[OrderFinished](_.key == orderId, after = followerLost)

      backupMaster = backup.startMaster(httpPort = Some(backupHttpPort)) await 99.s
      primaryMaster.eventWatch.await[Coupled](_.key == NoKey).head.eventId

      orderId = OrderId("🔸")
      primaryMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
      primaryMaster.eventWatch.await[OrderFinished](_.key == orderId, after = followerLost)
      backupMaster.eventWatch.await[OrderFinished](_.key == orderId, after = followerLost)

      primaryMaster.terminate() await 99.s
      backupMaster.terminate() await 99.s
    }
  }
}
