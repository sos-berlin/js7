package js7.tests.controller.proxy

import java.net.URI
import js7.base.auth.Admission
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver}
import js7.data.controller.ControllerCommand.ShutDown.ClusterAction
import js7.data.controller.ControllerCommand.{ClusterSwitchOver, ShutDown}
import js7.data.controller.ControllerEvent.ControllerReady
import js7.data.event.{EventId, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data_for_java.auth.{JAdmission, JHttpsConfig}
import js7.proxy.data.event.EventAndState
import js7.tests.controller.proxy.ClusterProxyTest.{backupUserAndPassword, primaryUserAndPassword, workflow}
import monix.execution.Scheduler.Implicits.traced
import scala.jdk.CollectionConverters.*

final class JournaledProxySwitchOverClusterTest extends OurTestSuite with ClusterProxyTest
{
  override protected val removeObsoleteJournalFiles = false

  "JournaledProxy accesses a switching Cluster" in {
    withControllerAndBackup() { (primary, _, backup, _, _) =>
      val backupController = backup.newController(httpPort = Some(backupControllerPort))

      lazy val proxy = controllerApi.startProxy().await(99.s)
      var lastEventId = EventId.BeforeFirst

      def runOrder(orderId: OrderId): Unit = {
        val whenFinished = proxy.observable
          .find {
            case EventAndState(Stamped(_, _, KeyedEvent(`orderId`, _: OrderFinished)), _, _) => true
            case _ => false
          }
          .timeoutOnSlowUpstream(99.s)
          .headL.runToFuture
        controllerApi.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
        whenFinished await 99.s
      }

      try {
        primary.runController(httpPort = Some(primaryControllerPort)) { primaryController =>
          val admissions = List(
            Admission(primaryController.localUri, Some(primaryUserAndPassword)),
            Admission(backupController.localUri, Some(backupUserAndPassword)))

          // Run a non-cluster test
          autoClosing(
            new JControllerFluxTester(admissions.map(JAdmission.apply).asJava, JHttpsConfig.empty)
          ) { _.test() }

          runOrder(OrderId("ORDER-ON-PRIMARY"))

          // SWITCH-OVER

          lastEventId = primaryController.eventWatch.lastAddedEventId
          primaryController.api.executeCommand(ClusterSwitchOver()).await(99.s).orThrow
          primaryController.terminated await 99.s
        }

        // Try to confuse controllerApi about the active controller and start primary controller again
        primary.runController(
          httpPort = Some(primaryControllerPort),
          dontWaitUntilReady = true
        ) { primaryController =>
          backupController.eventWatch.await[ControllerReady](after = lastEventId)
          primaryController.eventWatch.await[ClusterCoupled](after = lastEventId)
          runOrder(OrderId("ORDER-ON-BACKUP-1"))

          // RESTART BACKUP
          lastEventId = backupController.eventWatch.lastAddedEventId
          backupController.terminate().await(99.s)

          backup.runController(
            httpPort = Some(new URI(backupController.localUri.toString).getPort)
          ) { backupController2 =>
            runOrder(OrderId("ORDER-ON-BACKUP-RESTARTED"))
            backupController2.api.executeCommand(ShutDown(clusterAction = Some(ClusterAction.Failover)))
              .await(99.s).orThrow
            backupController2.terminated.await(99.s)
          }

          primaryController.eventWatch.await[ClusterFailedOver](after = lastEventId)
          runOrder(OrderId("ORDER-AFTER-FAILOVER"))
        }
      } finally
        proxy.stop.runToFuture await 99.s
    }
  }
}
