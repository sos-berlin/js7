package com.sos.jobscheduler.tests

import akka.actor.ActorRefFactory
import com.sos.jobscheduler.agent.scheduler.job.{JobConfiguration, JobScript}
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.https.KeyStoreRef
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.OperatingSystem.operatingSystem
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tests.HttpsServerSideTest._
import com.typesafe.config.ConfigUtil.quoteString
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.duration._

/**
  * Master and Agent with server-side HTTPS.
  *
  * @author Joacim Zschimmer
  */
class HttpsServerSideTest extends FreeSpec with BeforeAndAfterAll with DirectoryProvider.ForScalaTest {

  override protected lazy val masterHttpPort = None
  override protected lazy val masterHttpsPort = Some(findRandomFreeTcpPort())
  private lazy val masterApi = new AkkaHttpMasterApi(master.localUri,
    keyStoreRef = Some(KeyStoreRef(DirectoryProvider.MasterTrustStoreResource.url, SecretString("jobscheduler"))))

  protected val agentPaths = AgentPath("/TEST-AGENT") :: Nil
  override protected def agentHttps = true

  private lazy val eventCollector = new TestEventCollector

  override def beforeAll() = {
    // Reference to agents implicitly starts them (before master)
    directoryProvider.agents(0).provideHttpsCertificate()
    provideAgentConfiguration(directoryProvider.agents(0))
    assert(agents.forall(_.localUri.scheme == "https"))

    directoryProvider.master.provideMastersHttpsCertificate()
    provideMasterConfiguration(directoryProvider.master)
    eventCollector.start(master.injector.instance[ActorRefFactory], master.injector.instance[StampedKeyedEventBus])
    assert(master.localUri.scheme == "https")

    super.beforeAll()
  }

  "Login" in {
    masterApi.login(Some(UserId("TEST-USER") → SecretString("TEST-PASSWORD"))) await 99.s
  }

  "Run a job" in {
    masterApi.addOrder(FreshOrder(OrderId("TEST"), WorkflowPath("/TEST-WORKFLOW"))) await 99.s
    eventCollector.await[OrderFinished]()
  }

  "Terminate" in {
    masterApi.executeCommand(MasterCommand.Terminate) await 99.s
    masterApi.clearSession()  // To avoid automatic logoff because Master is terminating now.
  }
}

object HttpsServerSideTest
{
  private def provideAgentConfiguration(agent: DirectoryProvider.AgentTree): Unit = {
    (agent.config / "private/private.conf").append(
      s"""jobscheduler.auth.users {
         |  Master = ${quoteString("plain:" + agent.password.string)}
         |}
         |""".stripMargin)
    agent.writeJson(JobConfiguration(
      JobPath("/A") % VersionId.Anonymous,
      JobScript(operatingSystem.sleepingShellScript(0.seconds))))
  }

  private def provideMasterConfiguration(master: DirectoryProvider.Tree): Unit = {
    (master.config / "private/private.conf").append(
      """jobscheduler.auth.users {
        |  TEST-USER: "plain:TEST-PASSWORD"
        |}
        |""".stripMargin)
    master.writeTxt(WorkflowPath("/TEST-WORKFLOW"), """job "/A" on "/TEST-AGENT";""")
  }
}
