package js7.tests.controller.cluster

import js7.base.auth.UserId
import js7.base.generic.SecretString
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.controller.RunningController
import js7.controller.data.ControllerCommand.ClusterSwitchOver
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterSwitchedOver}
import js7.data.event.EventId
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.tests.controller.cluster.ControllerClusterTester._
import js7.tests.controller.cluster.SwitchOverClusterTest._
import monix.execution.Scheduler.Implicits.global
import scala.util.Try

final class SwitchOverClusterTest extends ControllerClusterTester
{
  override protected def removeObsoleteJournalFiles = false
  private lazy val manyOrdersCount = sys.props.get("SwitchOverClusterTest").map(_.toInt) getOrElse 1
  private lazy val timeout = if (manyOrdersCount > 0) 1.h else 99.s

  "Switchover" in {
    val orderIds = for (i <- 1 to manyOrdersCount) yield
      OrderId(s"ORDER-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-XXXXXXXXX-$i")
    val primaryHttpPort :: backupHttpPort :: Nil = findFreeTcpPorts(2)
    withControllerAndBackup(primaryHttpPort, backupHttpPort) { (primary, backup) =>
      var lastEventId = EventId.BeforeFirst
      backup.runController(httpPort = Some(backupHttpPort), dontWaitUntilReady = true) { backupController =>
        primary.runController(httpPort = Some(primaryHttpPort)) { implicit primaryController =>
          primaryController.eventWatch.await[ClusterCoupled]()
          val orderId = OrderId("⭕")
          primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.path))
          primaryController.httpApiDefaultLogin(Some(UserId("TEST") -> SecretString("TEST-PASSWORD")))
          addOrders(orderIds)
          primaryController.eventWatch.await[OrderProcessingStarted](_.key == orderId)
          backupController.eventWatch.await[OrderProcessingStarted](_.key == orderId)

          //if (manyOrdersCount > 1) {
          //  primaryController.executeCommandAsSystemUser(TakeSnapshot).await(timeout)
          //}

          // SWITCH OVER TO BACKUP
          primaryController.executeCommandAsSystemUser(ClusterSwitchOver).await(timeout).orThrow
          //May already be terminated: primaryController.eventWatch.await[ClusterSwitchedOver]()
          backupController.eventWatch.await[ClusterSwitchedOver]()
          // Controller terminates after switched-over
          for (t <- Try(primaryController.terminated.await(timeout)).failed) logger.error(s"Controller terminated. $t")

          //backupController.eventWatch.await[ClusterSwitchedOver](timeout = timeout)
          lastEventId = backupController.eventWatch.await[OrderFinished](_.key == orderId, timeout = timeout).head.eventId
        }

        assert(!backupController.journalActorState.isRequiringClusterAcknowledgement)

        // Start again the passive primary node
        primary.runController(httpPort = Some(primaryHttpPort), dontWaitUntilReady = true) { primaryController =>
          backupController.eventWatch.await[ClusterCoupled](after = lastEventId, timeout = timeout)
          primaryController.eventWatch.await[ClusterCoupled](after = lastEventId)
          assert(backupController.journalActorState.isRequiringClusterAcknowledgement)

          val orderId = OrderId("🔴")
          backupController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
          backupController.eventWatch.await[OrderProcessingStarted](_.key == orderId, timeout = timeout)
          primaryController.eventWatch.await[OrderProcessingStarted](_.key == orderId)

          // SWITCH OVER TO PRIMARY
          backupController.executeCommandAsSystemUser(ClusterSwitchOver).await(timeout).orThrow
          primaryController.eventWatch.await[ClusterSwitchedOver]()
          Try(backupController.terminated.await(timeout)).failed foreach { t =>
            logger.error(s"Controller terminated. $t")  // Erstmal beendet sich der Controller nach SwitchOver
          }
          assert(!primaryController.journalActorState.isRequiringClusterAcknowledgement)
          lastEventId = primaryController.eventWatch.await[OrderFinished](_.key == orderId, timeout = timeout).head.eventId

          locally {
            val orderId = OrderId("❌")
            primaryController.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
            primaryController.eventWatch.await[OrderProcessingStarted](_.key == orderId, timeout = timeout)
          }
        }
      }
    }
  }

  private def addOrders(orderId: Seq[OrderId])(implicit controller: RunningController): Unit = {
    controller.httpApi.login(onlyIfNotLoggedIn = true).await(timeout)
    orderId.grouped(1000)
      .map(_.map(FreshOrder(_, TestWorkflow.path, Some(Timestamp("3000-01-01T00:00:00Z")))))
      .foreach { orders =>
        controller.httpApi.addOrders(orders).await(timeout)
      }
  }
}

object SwitchOverClusterTest
{
  private val logger = Logger(getClass)
}
