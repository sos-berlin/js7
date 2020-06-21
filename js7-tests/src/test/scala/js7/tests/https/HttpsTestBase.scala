package js7.tests.https

import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigUtil.quoteString
import java.nio.file.Files.{createTempFile, delete}
import js7.base.auth.UserId
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax.{RichClosersAny, RichClosersAutoCloseable}
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.ScalazStyle._
import js7.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import js7.common.akkautils.ProvideActorSystem
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.syntax.RichPath
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.system.OperatingSystem.operatingSystem
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.common.utils.JavaResource
import js7.controller.client.AkkaHttpControllerApi
import js7.data.agent.AgentRefPath
import js7.data.job.ExecutablePath
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.tests.https.HttpsTestBase._
import js7.tests.testenv.{ControllerAgentForScalaTest, DirectoryProvider}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * Controller and Agent with server-side HTTPS.
  *
  * @author Joacim Zschimmer
  */
private[https] trait HttpsTestBase extends AnyFreeSpec with BeforeAndAfterAll with ControllerAgentForScalaTest with ProvideActorSystem
{
  override protected final def provideAgentHttpsCertificate = true
  protected def provideControllerClientCertificate = false
  protected def useCluster = true

  override protected final lazy val controllerHttpPort = None
  override protected final lazy val controllerHttpsPort = Some(findFreeTcpPort())
  override protected final def controllerClientCertificate = provideControllerClientCertificate ? ExportedClientTrustStoreResource
  private final lazy val agentHttpsPort = findFreeTcpPort()
  override protected final lazy val agentPorts = agentHttpsPort :: Nil
  private lazy val backupHttpsPort = findFreeTcpPort()
  override protected lazy val config = ConfigFactory.empty  // for ProviderActorSystem
  override protected lazy val controllerConfig = ConfigFactory.parseString(
    (useCluster ?: s"""
      js7.journal.cluster.nodes.Primary: "https://localhost:${controllerHttpsPort.get}"
      js7.journal.cluster.nodes.Backup: "https://localhost:$backupHttpsPort"
      js7.journal.cluster.watches = [ "https://localhost:$agentHttpsPort" ]
      """) + """
    js7.auth.users.Controller.password = "plain:PRIMARY-CONTROLLER-PASSWORD"
    js7.auth.users.TEST.password = "plain:TEST-PASSWORD"
    js7.auth.cluster.password = "BACKUP-CONTROLLER-PASSWORD"
    """)

  private lazy val clientKeyStore = createTempFile(getClass.getSimpleName + "-keystore-", ".p12")
  private lazy val controllerTrustStore = createTempFile(getClass.getSimpleName + "-primary-truststore-", ".p12")

  private lazy val backupDirectoryProvider = new DirectoryProvider(
    Nil,
    controllerConfig = ConfigFactory.parseString(s"""
      js7.journal.cluster.node.is-backup = yes
      js7.journal.cluster.watches = [ "https://localhost:$agentHttpsPort" ]
      js7.auth.users.Controller.password = "plain:BACKUP-CONTROLLER-PASSWORD"
      js7.auth.users.TEST.password = "plain:TEST-PASSWORD"
      js7.auth.cluster.password = "PRIMARY-CONTROLLER-PASSWORD"
      """),
    controllerHttpsMutual = provideControllerClientCertificate,
    controllerClientCertificate = provideControllerClientCertificate ? DirectoryProvider.ExportedControllerTrustStoreResource,
    agentHttps = true,
    agentHttpsMutual = provideControllerClientCertificate,
    testName = Some(getClass.simpleScalaName + "-Backup"))
  protected final lazy val backupController = backupDirectoryProvider.startController(httpPort = None, httpsPort = Some(backupHttpsPort)) await 99.s

  protected lazy val controllerApi = new AkkaHttpControllerApi(
    controller.localUri,
    Some(UserId("TEST-USER") -> SecretString("TEST-PASSWORD")),
    actorSystem,
    config,
    keyStoreRef = Some(KeyStoreRef(clientKeyStore,
      storePassword = SecretString("jobscheduler"),
      keyPassword = SecretString("jobscheduler"))),
    trustStoreRefs = TrustStoreRef(controllerTrustStore, SecretString("jobscheduler")) :: Nil,
  ).closeWithCloser

  override protected final def agentHttps = true
  protected final val agentRefPaths = AgentRefPath("/TEST-AGENT") :: Nil
  protected final val fileBased = TestWorkflow :: Nil

  override def beforeAll() = {
    clientKeyStore := ClientKeyStoreResource.contentBytes
    controllerTrustStore := DirectoryProvider.ExportedControllerTrustStoreResource.contentBytes
    provideCertificatesToCluster()

    provideAgentConfiguration(directoryProvider.agents(0))

    directoryProvider.controller.provideHttpsCertificates()
    provideControllerConfiguration(directoryProvider.controller)
    // We have provided our own certificates: backupDirectoryProvider.controller.provideHttpsCertificates()

    super.beforeAll()
  }

  private def provideCertificatesToCluster(): Unit = {
    backupDirectoryProvider.controller.configDir / "private/https-keystore.p12" := BackupKeyStoreResource.contentBytes
    backupDirectoryProvider.controller.configDir / "private/private.conf" ++= s"""
       |js7.https.truststores += {
       |  file = "$controllerTrustStore"
       |  store-password = "jobscheduler"
       |}
       |""".stripMargin

    val backupTrustStore = createTempFile(getClass.getSimpleName + "-backup-truststore-", ".p12") withCloser delete
    backupTrustStore := ExportedBackupTrustStoreResource.contentBytes
    directoryProvider.controller.configDir / "private/private.conf" ++= s"""
       |js7.https.truststores += {
       |  file = "$backupTrustStore"
       |  store-password = "jobscheduler"
       |}
       |""".stripMargin
  }

  override def afterAll() = {
    close()
    delete(controllerTrustStore)
    delete(clientKeyStore)
    super.afterAll()
    if (useCluster) backupController.terminate() await 99.s
    backupDirectoryProvider.close()
  }
}

private[https] object HttpsTestBase
{
  // Following resources have been generated with the command line:
  // js7-common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh \
  // -host=localhost -alias=client -config-directory=js7-tests/src/test/resources/js7/tests/https/resources -prefix="client-"
  private val ClientKeyStoreResource = JavaResource("js7/tests/https/resources/private/client-https-keystore.p12")
  private val ExportedClientTrustStoreResource = JavaResource("js7/tests/https/resources/export/client-https-truststore.p12")
  // js7-common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh \
  // -host=localhost -alias=backup-controller -config-directory=js7-tests/src/test/resources/js7/tests/https/resources -prefix="backup-controller-"
  private val BackupKeyStoreResource = JavaResource("js7/tests/https/resources/private/backup-controller-https-keystore.p12")
  private val ExportedBackupTrustStoreResource = JavaResource("js7/tests/https/resources/export/backup-controller-https-truststore.p12")

  private val TestWorkflow = WorkflowParser.parse(WorkflowPath("/TEST-WORKFLOW"), s"""
    define workflow {
      execute executable="/TEST$sh", agent="/TEST-AGENT";
    }""").orThrow

  private def provideAgentConfiguration(agent: DirectoryProvider.AgentTree): Unit = {
    (agent.configDir / "private/private.conf").append(
      s"""js7.auth.users {
         |  Controller = ${quoteString("plain:" + agent.password.string)}
         |}
         |""".stripMargin)
    agent.writeExecutable(ExecutablePath(s"/TEST$sh"), operatingSystem.sleepingShellScript(0.seconds))
  }

  private def provideControllerConfiguration(controller: DirectoryProvider.ControllerTree): Unit =
    controller.configDir / "private/private.conf" ++= """
      |js7.auth.users {
      |  TEST-USER: "plain:TEST-PASSWORD"
      |}
      |""".stripMargin
}
