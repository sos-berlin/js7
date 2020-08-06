package js7.tests.controller.proxy

import java.net.URI
import js7.base.auth.Admission
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.data.ControllerCommand
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.proxy.javaapi.JAdmission
import js7.proxy.javaapi.data.JHttpsConfig
import js7.proxy.{ControllerApi, EventAndState}
import js7.tests.controller.proxy.ClusterProxyTest.{backupUserAndPassword, primaryUserAndPassword}
import js7.tests.controller.proxy.JournaledProxyTest.workflow
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.jdk.CollectionConverters._

final class JournaledProxySwitchOverClusterTest extends AnyFreeSpec with ClusterProxyTest
{
  protected def inventoryItems = workflow :: Nil

  "JournaledProxy accesses a switching Cluster" in {
    runControllerAndBackup() { (_, primaryController, backup, backupController) =>
      val admissions = List(
        Admission(primaryController.localUri, Some(primaryUserAndPassword)),
        Admission(backupController.localUri, Some(backupUserAndPassword)))

      // Run a non-cluster test
      autoClosing(new JControllerFluxTester(admissions.map(JAdmission.apply).asJava, JHttpsConfig.empty)) { tester =>
        tester.test()
      }

      implicit val actorSystem = newActorSystem("JournaledProxySwitchOverClusterTest")
      val apiResources = for ((a, i) <- admissions.zipWithIndex)
        yield AkkaHttpControllerApi.resource(a.uri, a.userAndPassword, name = s"JournaledProxySwitchOverClusterTest-Controller-$i")
      val api = new ControllerApi(apiResources)
      val proxy = api.startProxy().await(99.s)
      try {
        def runOrder(orderId: OrderId): Unit = {
          val whenFinished = proxy.observable
            .find {
              case EventAndState(Stamped(_, _, KeyedEvent(`orderId`, _: OrderFinished)), _) => true
              case _ => false
            }
            .timeoutOnSlowUpstream(99.s)
            .headL.runToFuture
          api.addOrder(FreshOrder(orderId, JournaledProxyTest.workflow.path)).await(99.s).orThrow
          whenFinished await 99.s
        }

        runOrder(OrderId("ORDER-ON-PRIMARY"))

        // SWITCH-OVER

        primaryController.executeCommandAsSystemUser(ControllerCommand.ClusterSwitchOver).await(99.s).orThrow
        primaryController.terminated await 99.s
        primaryController.close()
        runOrder(OrderId("ORDER-ON-BACKUP-1"))

        // RESTART BACKUP

        backupController.terminate().await(99.s)
        backupController.close()
        //backupController.waitUntilReady()
        val backupController2 = backup.startController(httpPort = Some(new URI(backupController.localUri.toString).getPort)) await 99.s
        runOrder(OrderId("ORDER-ON-BACKUP-RESTARTED"))
        backupController2.terminate(suppressSnapshot = true).await(99.s)
      } finally {
        proxy.stop.runToFuture await 99.s
        Akkas.terminateAndWait(actorSystem)
      }
    }
  }
}
