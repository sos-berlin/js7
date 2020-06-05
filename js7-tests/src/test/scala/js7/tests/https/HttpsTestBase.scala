package js7.tests.https

import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigUtil.quoteString
import java.nio.file.Files.{createTempFile, delete}
import js7.base.auth.UserId
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.ScalazStyle._
import js7.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import js7.common.akkautils.ProvideActorSystem
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.syntax.RichPath
import js7.common.system.OperatingSystem.operatingSystem
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.common.utils.JavaResource
import js7.data.agent.AgentRefPath
import js7.data.job.ExecutablePath
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.master.client.AkkaHttpMasterApi
import js7.tests.https.HttpsTestBase._
import js7.tests.testenv.{DirectoryProvider, MasterAgentForScalaTest}
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

  protected lazy val masterApi = new AkkaHttpMasterApi(
    master.localUri,
    Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD")),
    actorSystem,
    config,
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
  // common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=client -config-directory=tests/src/test/resources/js7/tests/client
  private val ClientKeyStoreResource = JavaResource("js7/tests/client/private/https-keystore.p12")
  private val ClientTrustStoreResource = JavaResource("js7/tests/client/export/https-truststore.p12")

  private val TestWorkflow = WorkflowParser.parse(WorkflowPath("/TEST-WORKFLOW"), s"""
    define workflow {
      execute executable="/TEST$sh", agent="/TEST-AGENT";
    }""").orThrow

  private def provideAgentConfiguration(agent: DirectoryProvider.AgentTree): Unit = {
    (agent.configDir / "private/private.conf").append(
      s"""js7.auth.users {
         |  Master = ${quoteString("plain:" + agent.password.string)}
         |}
         |""".stripMargin)
    agent.writeExecutable(ExecutablePath(s"/TEST$sh"), operatingSystem.sleepingShellScript(0.seconds))
  }

  private def provideMasterConfiguration(master: DirectoryProvider.MasterTree): Unit = {
    (master.configDir / "private/private.conf").append("""
      |js7.auth.users {
      |  TEST-USER: "plain:TEST-PASSWORD"
      |}
      |""".stripMargin)
  }
}
