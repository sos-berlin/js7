package js7.tests.controller.proxy

import java.net.URI
import js7.base.auth.Admission
import js7.base.problem.Checked.Ops
import js7.base.thread.Futures.implicits._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerEvent.ControllerReady
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data_for_java.auth.{JAdmission, JHttpsConfig}
import js7.proxy.ControllerApi
import js7.proxy.data.event.EventAndState
import js7.tests.controller.proxy.ClusterProxyTest.{backupUserAndPassword, primaryUserAndPassword, workflow}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.jdk.CollectionConverters._

final class JournaledProxySwitchOverClusterTest extends AnyFreeSpec with ClusterProxyTest
{
  "JournaledProxy accesses a switching Cluster" in {
    runControllerAndBackup() { (_, primaryController, backup, backupController, _) =>
      primaryController.waitUntilReady()
      val admissions = List(
        Admission(primaryController.localUri, Some(primaryUserAndPassword)),
        Admission(backupController.localUri, Some(backupUserAndPassword)))

      // Run a non-cluster test
      autoClosing(new JControllerFluxTester(admissions.map(JAdmission.apply).asJava, JHttpsConfig.empty)) { tester =>
        tester.test()
      }

      val api = new ControllerApi(apiResources)
      val proxy = api.startProxy().await(99.s)
      try {
        def runOrder(orderId: OrderId): Unit = {
          val whenFinished = proxy.observable
            .find {
              case EventAndState(Stamped(_, _, KeyedEvent(`orderId`, _: OrderFinished)), _, _) => true
              case _ => false
            }
            .timeoutOnSlowUpstream(99.s)
            .headL.runToFuture
          api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
          whenFinished await 99.s
        }

        runOrder(OrderId("ORDER-ON-PRIMARY"))

        // SWITCH-OVER

        val eventId = primaryController.eventWatch.lastAddedEventId
        primaryController.executeCommandAsSystemUser(ControllerCommand.ClusterSwitchOver).await(99.s).orThrow
        primaryController.terminated await 99.s
        primaryController.close()
        backupController.eventWatch.await[ControllerReady](after = eventId)
        runOrder(OrderId("ORDER-ON-BACKUP-1"))

        // RESTART BACKUP

        backupController.terminate().await(99.s)
        backupController.close()
        val backupController2 = backup.startController(httpPort = Some(new URI(backupController.localUri.toString).getPort)) await 99.s
        backupController2.waitUntilReady()
        runOrder(OrderId("ORDER-ON-BACKUP-RESTARTED"))
        backupController2.terminate(suppressSnapshot = true).await(99.s)
      } finally
        proxy.stop.runToFuture await 99.s
    }
  }
}
