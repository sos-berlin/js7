package com.sos.jobscheduler.tests.master.cluster

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import com.sos.jobscheduler.data.cluster.ClusterEvent
import com.sos.jobscheduler.data.cluster.ClusterEvent.FailedOver
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.tests.master.cluster.MasterClusterTester._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.Deadline.now

final class FailoverClusterTest extends MasterClusterTester
{
  "Failover" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      val primaryMaster = primary.startMaster(httpPort = Some(primaryHttpPort)) await 99.s
      val backupMaster = backup.startMaster(httpPort = Some(backupHttpPort)) await 99.s
      primaryMaster.eventWatch.await[ClusterEvent.ClusterCoupled]()

      val t = now
      val sleepWhileFailing = 7.s  // Failover takes some seconds anyway
      val orderId = OrderId("💥")
      primaryMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path, arguments = Map("SLEEP" -> sleepWhileFailing.toSeconds.toString)))
      primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)
      backupMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)
      // KILL PRIMARY
      primaryMaster.terminate() await 99.s
      assert(now < t + sleepWhileFailing, "The shell script should run while Master fails")

      val failedOver = backupMaster.eventWatch.await[FailedOver](_.key == NoKey).head.eventId

      backupMaster.eventWatch.await[OrderFinished](_.key == orderId, after = failedOver)
      backupMaster.terminate() await 99.s
    }
  }
}
