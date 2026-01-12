package js7.tests.controller.proxy

import cats.effect.unsafe.IORuntime
import js7.base.auth.Admission
import js7.base.monixlike.MonixLikeExtensions.headL
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
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
import scala.jdk.CollectionConverters.*

final class JournaledProxySwitchOverClusterTest extends OurTestSuite, ClusterProxyTest:

  override protected val removeObsoleteJournalFiles = false

  private given IORuntime = ioRuntime

  "JournaledProxy accesses a switching Cluster" in:
    withControllerAndBackup(): (primary, _, backup, _, _) =>
      val backupController = backup.newController()

      lazy val (proxy, releaseProxy) = controllerApi.controllerProxy().allocated.await(99.s)
      var lastEventId = EventId.BeforeFirst

      def runOrder(orderId: OrderId): Unit =
        val whenFinished = proxy.stream()
          .find:
            case EventAndState(Stamped(_, _, KeyedEvent(`orderId`, _: OrderFinished)), _, _) =>
              true
            case _ => false
          .timeoutOnPull(99.s)
          .headL.unsafeToFuture()
        controllerApi.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
        whenFinished.await(99.s)

      try
        primary.runController(): primaryController =>
          primaryController.await[ClusterCoupled]()

          val admissions = List(
            Admission(primaryController.localUri, Some(primaryUserAndPassword)),
            Admission(backupController.localUri, Some(backupUserAndPassword)))

          // Run a non-cluster test
          autoClosing(
            new JControllerFluxTester(admissions.map(JAdmission.apply).asJava, JHttpsConfig.empty)
          ) { _.test() }

          runOrder(OrderId("ORDER-AT-PRIMARY"))

          // SWITCH-OVER

          lastEventId = primaryController.lastAddedEventId
          primaryController.execCmd:
            ClusterSwitchOver()

        // Try to confuse controllerApi about which controller is active,
        // and start the primary controller again
        primary.runController(dontWaitUntilReady = true): primaryController =>
          backupController.await[ControllerReady](after = lastEventId)
          primaryController.await[ClusterCoupled](after = lastEventId)
          runOrder(OrderId("ORDER-AT-BACKUP-1"))

          // RESTART BACKUP
          lastEventId = backupController.eventWatch.lastAddedEventId
          backupController.terminate().await(99.s)

          backup.runController(): backupController2 =>
            runOrder(OrderId("ORDER-AT-BACKUP-RESTARTED"))
            try
              backupController2.execCmd:
                ShutDown(clusterAction = Some(ClusterAction.Failover))
            catch case t: org.apache.pekko.pattern.AskTimeoutException =>
              // Due to dirty test shutdown behaviour?
              Logger.warn(s"Shutdown with failover: $t", t)

          primaryController.await[ClusterFailedOver](after = lastEventId)
          runOrder(OrderId("ORDER-AFTER-FAILOVER"))
      finally
        releaseProxy.await(99.s)
