package com.sos.jobscheduler.tests.https

import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.Closer.syntax.RichClosersAutoCloseable
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax.RichPath
import com.sos.jobscheduler.common.system.OperatingSystem.operatingSystem
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.tests.https.HttpsTestBase._
import com.sos.jobscheduler.tests.testenv.{DirectoryProvider, MasterAgentForScalaTest}
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigUtil.quoteString
import java.nio.file.Files.{createTempFile, delete}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * Master and Agent with server-side HTTPS.
  *
  * @author Joacim Zschimmer
  */
private[https] trait HttpsTestBase extends AnyFreeSpec with BeforeAndAfterAll with MasterAgentForScalaTest with ProvideActorSystem
{
  override protected final def provideAgentHttpsCertificate = true
  protected def provideMasterClientCertificate = false

  override protected final lazy val masterHttpPort = None
  override protected final lazy val masterHttpsPort = Some(findFreeTcpPort())
  override protected final def masterClientCertificate = provideMasterClientCertificate ? ClientTrustStoreResource
  protected val config = ConfigFactory.empty

  private lazy val keyStore = createTempFile(getClass.getSimpleName + "-keystore-", ".p12")
  private lazy val trustStore = createTempFile(getClass.getSimpleName + "-truststore-", ".p12")

  protected lazy val masterApi = AkkaHttpMasterApi(
    master.localUri, actorSystem, config,
    keyStoreRef = Some(KeyStoreRef(keyStore.toUri.toURL, storePassword = SecretString("jobscheduler"), keyPassword = SecretString("jobscheduler"))),
    trustStoreRef = Some(TrustStoreRef(trustStore.toUri.toURL, storePassword = SecretString("jobscheduler"))),
  ).closeWithCloser

  override protected def agentHttps = true
  protected val agentRefPaths = AgentRefPath("/TEST-AGENT") :: Nil
  protected val fileBased = TestWorkflow :: Nil

  override def beforeAll() = {
    // Reference to agents implicitly starts them (before master)
    provideAgentConfiguration(directoryProvider.agents(0))
    assert(agents.forall(_.localUri.string startsWith "https://"))

    directoryProvider.master.provideHttpsCertificate()
    provideMasterConfiguration(directoryProvider.master)
    assert(master.localUri.string startsWith "https://")

    keyStore := ClientKeyStoreResource.contentBytes
    trustStore := DirectoryProvider.MasterTrustStoreResource.contentBytes

    super.beforeAll()
  }

  override def afterAll() = {
    close()
    delete(keyStore)
    super.afterAll()
  }
}

private[https] object HttpsTestBase
{
  // Following resources have been generated with the command line:
  // common/src/main/resources/com/sos/jobscheduler/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=client -config-directory=tests/src/test/resources/com/sos/jobscheduler/tests/client
  private val ClientKeyStoreResource = JavaResource("com/sos/jobscheduler/tests/client/private/https-keystore.p12")
  private val ClientTrustStoreResource = JavaResource("com/sos/jobscheduler/tests/client/export/https-truststore.p12")

  private val TestWorkflow = WorkflowParser.parse(WorkflowPath("/TEST-WORKFLOW"), s"""
    define workflow {
      execute executable="/TEST$sh", agent="/TEST-AGENT";
    }""").orThrow

  private def provideAgentConfiguration(agent: DirectoryProvider.AgentTree): Unit = {
    (agent.configDir / "private/private.conf").append(
      s"""jobscheduler.auth.users {
         |  Master = ${quoteString("plain:" + agent.password.string)}
         |}
         |""".stripMargin)
    agent.writeExecutable(ExecutablePath(s"/TEST$sh"), operatingSystem.sleepingShellScript(0.seconds))
  }

  private def provideMasterConfiguration(master: DirectoryProvider.MasterTree): Unit = {
    (master.configDir / "private/private.conf").append("""
      |jobscheduler.auth.users {
      |  TEST-USER: "plain:TEST-PASSWORD"
      |}
      |""".stripMargin)
  }
}
