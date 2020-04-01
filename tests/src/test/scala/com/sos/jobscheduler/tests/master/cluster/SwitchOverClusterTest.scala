package com.sos.jobscheduler.tests.master.cluster

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import com.sos.jobscheduler.data.cluster.ClusterEvent
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.tests.master.cluster.MasterClusterTester._
import com.sos.jobscheduler.tests.master.cluster.SwitchOverClusterTest._
import monix.execution.Scheduler.Implicits.global
import scala.util.Try

final class SwitchOverClusterTest extends MasterClusterTester
{
  "Switchover" in {
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      var lastEventId = EventId.BeforeFirst
      backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupMaster =>
        primary.runMaster(httpPort = Some(primaryHttpPort)) { primaryMaster =>
          //primaryMaster.executeCommandAsSystemUser(
          //  ClusterStartBackupNode(primaryUri = Uri(primaryMaster.localUri), backupUri = Uri(backupMaster.localUri))
          //).await(99.s).orThrow
          primaryMaster.eventWatch.await[ClusterEvent.ClusterCoupled]()
          val orderId = OrderId("⭕")
          primaryMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
          primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)
          backupMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)

          // SWITCH OVER TO BACKUP
          primaryMaster.executeCommandAsSystemUser(MasterCommand.ClusterSwitchOver).await(99.s).orThrow
          primaryMaster.eventWatch.await[ClusterEvent.ClusterSwitchedOver]()
          for (t <- Try(primaryMaster.terminated.await(99.s)).failed) logger.error(s"Master terminated. $t")   // Erstmal beendet sich der Master nach SwitchOver

          backupMaster.eventWatch.await[ClusterEvent.ClusterSwitchedOver]()
          lastEventId = backupMaster.eventWatch.await[OrderFinished](_.key == orderId).head.eventId
        }

        assert(!backupMaster.journalActorState.isRequiringClusterAcknowledgement)

        // Start again the passive primary node
        primary.runMaster(httpPort = Some(primaryHttpPort), dontWaitUntilReady = true) { primaryMaster =>
          backupMaster.eventWatch.await[ClusterEvent.ClusterCoupled](after = lastEventId)
          primaryMaster.eventWatch.await[ClusterEvent.ClusterCoupled](after = lastEventId)
          assert(backupMaster.journalActorState.isRequiringClusterAcknowledgement)

          val orderId = OrderId("🔴")
          backupMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
          backupMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)
          primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)

          // SWITCH OVER TO PRIMARY
          backupMaster.executeCommandAsSystemUser(MasterCommand.ClusterSwitchOver).await(99.s).orThrow
          backupMaster.eventWatch.await[ClusterEvent.ClusterSwitchedOver]()
          Try(backupMaster.terminated.await(99.s)).failed foreach { t => logger.error(s"Master terminated. $t")}   // Erstmal beendet sich der Master nach SwitchOver
          assert(!primaryMaster.journalActorState.isRequiringClusterAcknowledgement)
          lastEventId = primaryMaster.eventWatch.await[OrderFinished](_.key == orderId).head.eventId

          locally {
            val orderId = OrderId("❌")
            primaryMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
            primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)
          }
        }
      }
    }
  }
}

object SwitchOverClusterTest {
  private val logger = Logger(getClass)
}
