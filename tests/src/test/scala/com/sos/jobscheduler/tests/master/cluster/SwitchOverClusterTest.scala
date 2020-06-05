package js7.tests.master.cluster

import js7.base.auth.UserId
import js7.base.generic.SecretString
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterSwitchedOver}
import js7.data.event.EventId
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.master.RunningMaster
import js7.master.data.MasterCommand.ClusterSwitchOver
import js7.tests.master.cluster.MasterClusterTester._
import js7.tests.master.cluster.SwitchOverClusterTest._
import monix.execution.Scheduler.Implicits.global
import scala.util.Try

final class SwitchOverClusterTest extends MasterClusterTester
{
  override protected def removeObsoleteJournalFiles = false
  private lazy val manyOrdersCount = sys.props.get("SwitchOverClusterTest").map(_.toInt) getOrElse 1
  private lazy val timeout = if (manyOrdersCount > 0) 1.h else 99.s

  "Switchover" in {
    val orderIds = for (i <- 1 to manyOrdersCount) yield
      OrderId(s"ORDER-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-$i")
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
          //  primaryMaster.executeCommandAsSystemUser(TakeSnapshot).await(timeout)
          //}

          // SWITCH OVER TO BACKUP
          primaryMaster.executeCommandAsSystemUser(ClusterSwitchOver).await(timeout).orThrow
          //May already be terminated: primaryMaster.eventWatch.await[ClusterSwitchedOver]()
          backupMaster.eventWatch.await[ClusterSwitchedOver]()
          // Master terminates after switched-over
          for (t <- Try(primaryMaster.terminated.await(timeout)).failed) logger.error(s"Master terminated. $t")

          //backupMaster.eventWatch.await[ClusterSwitchedOver](timeout = timeout)
          lastEventId = backupMaster.eventWatch.await[OrderFinished](_.key == orderId, timeout = timeout).head.eventId
        }

        assert(!backupMaster.journalActorState.isRequiringClusterAcknowledgement)

        // Start again the passive primary node
        primary.runMaster(httpPort = Some(primaryHttpPort), dontWaitUntilReady = true) { primaryMaster =>
          backupMaster.eventWatch.await[ClusterCoupled](after = lastEventId, timeout = timeout)
          primaryMaster.eventWatch.await[ClusterCoupled](after = lastEventId)
          assert(backupMaster.journalActorState.isRequiringClusterAcknowledgement)

          val orderId = OrderId("🔴")
          backupMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
          backupMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId, timeout = timeout)
          primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId)

          // SWITCH OVER TO PRIMARY
          backupMaster.executeCommandAsSystemUser(ClusterSwitchOver).await(timeout).orThrow
          primaryMaster.eventWatch.await[ClusterSwitchedOver]()
          Try(backupMaster.terminated.await(timeout)).failed foreach { t =>
            logger.error(s"Master terminated. $t")  // Erstmal beendet sich der Master nach SwitchOver
          }
          assert(!primaryMaster.journalActorState.isRequiringClusterAcknowledgement)
          lastEventId = primaryMaster.eventWatch.await[OrderFinished](_.key == orderId, timeout = timeout).head.eventId

          locally {
            val orderId = OrderId("❌")
            primaryMaster.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
            primaryMaster.eventWatch.await[OrderProcessingStarted](_.key == orderId, timeout = timeout)
          }
        }
      }
    }
  }

  private def addOrders(orderId: Seq[OrderId])(implicit master: RunningMaster): Unit = {
    master.httpApi.login(onlyIfNotLoggedIn = true).await(timeout)
    orderId.grouped(1000)
      .map(_.map(FreshOrder(_, TestWorkflow.path, Some(Timestamp("3000-01-01T00:00:00Z")))))
      .foreach { orders =>
        master.httpApi.addOrders(orders).await(timeout)
      }
  }
}

object SwitchOverClusterTest
{
  private val logger = Logger(getClass)
}
