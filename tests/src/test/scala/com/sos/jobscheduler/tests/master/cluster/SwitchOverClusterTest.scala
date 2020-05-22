package com.sos.jobscheduler.tests.master.cluster

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import com.sos.jobscheduler.data.cluster.ClusterEvent.{ClusterCoupled, ClusterSwitchedOver}
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand.{ClusterSwitchOver, TakeSnapshot}
import com.sos.jobscheduler.tests.master.cluster.MasterClusterTester._
import com.sos.jobscheduler.tests.master.cluster.SwitchOverClusterTest._
import monix.execution.Scheduler.Implicits.global
import scala.util.Try

final class SwitchOverClusterTest extends MasterClusterTester
{
  override protected def removeObsoleteJournalFiles = false
  private lazy val manyOrdersCount = sys.props.get("SwitchOverClusterTest").map(_.toInt) getOrElse 1
  private lazy val longTimeout = if (manyOrdersCount > 0) 1.h else 99.s

  "Switchover" in {
    val orderIds = for (i <- 1 to manyOrdersCount) yield OrderId(s"ORDER-$i")
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withMasterAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      var lastEventId = EventId.BeforeFirst
      backup.runMaster(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupMaster =>
        primary.runMaster(httpPort = Some(primaryHttpPort)) { implicit primaryMaster =>
          primaryMaster.eventWatch.await[ClusterCoupled]()
          val orderId = OrderId("⭕")
          primaryMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.path))
          primaryMaster.httpApiDefaultLogin(Some(UserId("TEST") -> SecretString("TEST-PASSWORD")))
          addOrders(orderIds)
          primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)
          backupMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)

          //if (manyOrdersCount > 1) {
          //  primaryMaster.executeCommandAsSystemUser(TakeSnapshot).await(longTimeout)
          //}

          // SWITCH OVER TO BACKUP
          primaryMaster.executeCommandAsSystemUser(ClusterSwitchOver).await(longTimeout).orThrow
          //May already be terminated: primaryMaster.eventWatch.await[ClusterSwitchedOver]()
          backupMaster.eventWatch.await[ClusterSwitchedOver]()
          // Master terminates after switched-over
          for (t <- Try(primaryMaster.terminated.await(99.s)).failed) logger.error(s"Master terminated. $t")

          //backupMaster.eventWatch.await[ClusterSwitchedOver](timeout = longTimeout)
          lastEventId = backupMaster.eventWatch.await[OrderFinished](_.key == orderId, timeout = longTimeout).head.eventId
        }

        assert(!backupMaster.journalActorState.isRequiringClusterAcknowledgement)

        // Start again the passive primary node
        primary.runMaster(httpPort = Some(primaryHttpPort), dontWaitUntilReady = true) { primaryMaster =>
          backupMaster.eventWatch.await[ClusterCoupled](after = lastEventId, timeout = longTimeout)
          primaryMaster.eventWatch.await[ClusterCoupled](after = lastEventId)
          assert(backupMaster.journalActorState.isRequiringClusterAcknowledgement)

          val orderId = OrderId("🔴")
          backupMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
          backupMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId, timeout = longTimeout)
          primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)

          // SWITCH OVER TO PRIMARY
          backupMaster.executeCommandAsSystemUser(ClusterSwitchOver).await(longTimeout).orThrow
          primaryMaster.eventWatch.await[ClusterSwitchedOver]()
          Try(backupMaster.terminated.await(99.s)).failed foreach { t =>
            logger.error(s"Master terminated. $t")  // Erstmal beendet sich der Master nach SwitchOver
          }
          assert(!primaryMaster.journalActorState.isRequiringClusterAcknowledgement)
          lastEventId = primaryMaster.eventWatch.await[OrderFinished](_.key == orderId, timeout = longTimeout).head.eventId

          locally {
            val orderId = OrderId("❌")
            primaryMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
            primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId, timeout = longTimeout)
          }
        }
      }
    }
  }

  private def addOrders(orderId: Seq[OrderId])(implicit master: RunningMaster): Unit = {
    master.httpApi.login(onlyIfNotLoggedIn = true).await(99.s)
    orderId.grouped(1000)
      .map(_.map(FreshOrder(_, TestWorkflow.path, Some(Timestamp("3000-01-01T00:00:00Z")))))
      .foreach { orders =>
        master.httpApi.addOrders(orders).await(99.s)
      }
  }
}

object SwitchOverClusterTest
{
  private val logger = Logger(getClass)
  private val longTimeout = 1.h
}
