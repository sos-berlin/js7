package js7.tests.controller.proxy

import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.web.Uri
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.configutils.Configs._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.data.ControllerCommand
import js7.data.agent.AgentRefPath
import js7.data.cluster.ClusterEvent.ClusterCoupled
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.proxy.javaapi.JAdmission
import js7.proxy.javaapi.data.JHttpsConfig
import js7.proxy.{ControllerApi, EventAndState}
import js7.tests.controller.proxy.JournaledProxyClusterTest._
import js7.tests.controller.proxy.JournaledProxyTest.{primaryCredentials, workflow}
import js7.tests.testenv.DirectoryProvider
import js7.tests.testenv.DirectoryProvider.script
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.jdk.CollectionConverters._

final class JournaledProxyClusterTest extends AnyFreeSpec
{
  "JournaledProxy access a switching Cluster" in {
    val primaryPort :: backupPort :: agentPort :: Nil = findFreeTcpPorts(3)
    val primaryDirectoryProvider = new DirectoryProvider(
      AgentRefPath("/AGENT") :: Nil,
      workflow :: Nil,
      controllerConfig = config"""
        js7.journal.cluster {
          nodes {
            Primary: "http://localhost:$primaryPort"
            Backup: "http://localhost:$backupPort"
          }
          watches = [ "http://127.0.0.1:$agentPort" ]
        }
        js7.auth.users {
          Controller {
            password = "plain:PRIMARY-CONTROLLER-PASSWORD"
          }
          Proxy {
            password = "plain:PROXYS-PASSWORD-FOR-PRIMARY"
          }
        }
        js7.auth.cluster.password = "BACKUP-CONTROLLER-PASSWORD"
        """,
      agentPorts = agentPort :: Nil,
      testName = Some("JournaledProxyClusterTest-Primary"))
    val agentTree = primaryDirectoryProvider.agents(0)
    agentTree.writeExecutable(ExecutablePath("/test.cmd"), script(1.s))

    val backupDirectoryProvider = new DirectoryProvider(
      Nil,
      controllerConfig = config"""
        js7.journal.cluster.node.is-backup = yes
        js7.journal.cluster.watches = [ "http://127.0.0.1:$agentPort" ]
        js7.auth.users {
          Controller {
            password = "plain:BACKUP-CONTROLLER-PASSWORD"
          }
          Proxy {
            password = "plain:PROXYS-PASSWORD-FOR-BACKUP"
          }
        }
        js7.auth.cluster.password = "PRIMARY-CONTROLLER-PASSWORD"
        js7.auth.agents."http://127.0.0.1:$agentPort" = "${agentTree.password.string}"
        js7.auth.agents."/AGENT" = "${agentTree.password.string}"
        """,
      testName = Some("JournaledProxyClusterTest-Backup"))

    autoClosing(backupDirectoryProvider) { _ =>
      val admissions = List(
        Admission(Uri(s"http://127.0.0.1:$primaryPort"), Some(primaryCredentials.underlying)),
        Admission(Uri(s"http://127.0.0.1:$backupPort"), Some(backupCredentials)))

      primaryDirectoryProvider.runAgents() { _ =>
        val controllers = {
          val primary :: backup :: Nil = Task.parSequence(List(
            primaryDirectoryProvider.startController(httpPort = Some(primaryPort)),
            backupDirectoryProvider.startController(httpPort = Some(backupPort)))
          ).await(99.s)
          primary.eventWatch.await[ClusterCoupled]()
          primary :: backup :: Nil
        }

        // Run a non-cluster test
        autoClosing(new JControllerFluxTester(admissions.map(JAdmission.apply).asJava, JHttpsConfig.empty)) { tester =>
          tester.test()
        }

        val primary = controllers(0)
        var backup = controllers(1)
        implicit val actorSystem = newActorSystem("JournaledProxyClusterTest")
        val apiResources = for ((a, i) <- admissions.zipWithIndex)
          yield AkkaHttpControllerApi.resource(a.uri, a.userAndPassword, name = s"JournaledProxy-$i")
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

          primary.executeCommandAsSystemUser(ControllerCommand.ClusterSwitchOver).await(99.s).orThrow
          primary.terminated await 99.s
          primary.close()
          runOrder(OrderId("ORDER-ON-BACKUP-1"))

          backup.terminate().await(99.s)
          backup.close()
          backup.waitUntilReady()
          backup = backupDirectoryProvider.startController(httpPort = Some(backupPort)) await 99.s
          runOrder(OrderId("ORDER-ON-BACKUP-RESTARTED"))
        } finally {
          proxy.stop.runToFuture await 99.s
          Akkas.terminateAndWait(actorSystem)
          backup.terminate() await 99.s
          backup.close()
        }
      }
    }
  }
}

object JournaledProxyClusterTest
{
  private val backupCredentials = UserAndPassword(UserId("Proxy"), SecretString("PROXYS-PASSWORD-FOR-BACKUP"))
}
