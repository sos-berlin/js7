package js7.tests.https

import cats.syntax.option._
import com.typesafe.config.ConfigFactory
import java.nio.file.Files.{createTempFile, delete}
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkahttp.https.KeyStoreRef
import js7.common.akkautils.ProvideActorSystem
import js7.common.configutils.Configs._
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.syntax.RichPath
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.system.ServerOperatingSystem.operatingSystem
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.common.utils.JavaResource
import js7.controller.client.AkkaHttpControllerApi
import js7.data.agent.AgentId
import js7.data.job.RelativePathExecutable
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.https.HttpsTestBase._
import js7.tests.testenv.DirectoryProvider.{ExportedControllerTrustStoreRef, ExportedControllerTrustStoreResource}
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
private[https] trait HttpsTestBase
extends AnyFreeSpec with BeforeAndAfterAll with ControllerAgentForScalaTest with ProvideActorSystem
{
  override protected final def provideAgentHttpsCertificate = true
  protected def provideControllerClientCertificate = false
  protected def useCluster = true

  override protected final lazy val controllerHttpPort = None
  override protected final lazy val controllerHttpsPort = Some(findFreeTcpPort())
  override protected final lazy val controllerTrustStores =
    provideControllerClientCertificate.thenList(ExportedClientTrustStoreResource) ::: ExportedBackupTrustStoreResource :: Nil
  private final lazy val agentHttpsPort = findFreeTcpPort()
  override protected final lazy val agentPorts = agentHttpsPort :: Nil
  private lazy val backupHttpsPort = findFreeTcpPort()
  override protected lazy val config = ConfigFactory.empty  // for ProviderActorSystem
  protected def extraDistringuishedNameUserAndPassword = none[UserAndPassword]

  protected final val otherUserAndPassword = UserAndPassword(UserId("OTHER") -> SecretString("OTHER-PASSWORD"))

  override protected lazy val controllerConfig = {
    val dn = "CN=Test client,DC=test-client,DC=HttpsTestBase,DC=tests,DC=js7,DC=sh"
    ((useCluster ?
      config"""
      js7.journal.cluster {
        nodes {
          Primary: "https://localhost:${controllerHttpsPort.get}"
          Backup: "https://localhost:$backupHttpsPort"
        }
        watches = [ "https://localhost:$agentHttpsPort" ]
      }""") ++
        Some(config"""
          js7.web.server.auth.invalid-authentication-delay = 10.ms
          js7.web.server.auth.https-client-authentication = $controllerHttpsMutual
          js7.auth.users {
            Controller {
              password = "plain:PRIMARY-CONTROLLER-PASSWORD"
              distinguished-names = [ "CN=Backup Controller,DC=backup-controller,DC=HttpsTestBase,DC=tests,DC=js7,DC=sh" ]
            }
            TEST {
              password = "plain:TEST-PASSWORD"
              distinguished-names = [ "$dn" ]
            }
            "${otherUserAndPassword.userId.string}" {
              password = "plain:${otherUserAndPassword.password.string}"
            }
          }
        """) ++
        extraDistringuishedNameUserAndPassword.map(uw => config"""
          js7.auth.users {
            "${uw.userId.string}" {
              password = "plain:${uw.password.string}"
              distinguished-names = [ "$dn" ]
            }
          }""") ++
        (!controllerHttpsMutual ? config"""
          js7.auth.cluster {
            user-id = "backup-controller"
            password = "BACKUP-CONTROLLER-PASSWORD"
          }""")
    ).reduce(_ withFallback _)
  }

  private lazy val clientKeyStore = createTempFile(getClass.getSimpleName + "-keystore-", ".p12")

  private lazy val backupDirectoryProvider = new DirectoryProvider(
    Nil,
    controllerConfig = config"""
      js7.journal.cluster.node.is-backup = yes
      js7.journal.cluster.watches = [ "https://localhost:$agentHttpsPort" ]
      js7.web.server.auth.https-client-authentication = $controllerHttpsMutual
      js7.auth.users {
        Controller {
          password = "plain:BACKUP-CONTROLLER-PASSWORD"
          distinguished-names = [ "CN=Primary Controller,DC=primary-controller,DC=DirectoryProvider,DC=tests,DC=js7,DC=sh" ]
        }
      }
    """.withFallback(
      if (controllerHttpsMutual)
        ConfigFactory.empty
      else config"""
        js7.auth.users.TEST.password = "plain:TEST-PASSWORD"
        js7.auth.cluster.password = "PRIMARY-CONTROLLER-PASSWORD" """),
    controllerKeyStore = Some(BackupKeyStoreResource),
    controllerTrustStores = ExportedControllerTrustStoreResource :: Nil,
    agentHttps = true,
    agentHttpsMutual = provideControllerClientCertificate,
    testName = Some(getClass.simpleScalaName + "-Backup"))

  protected final lazy val backupController = backupDirectoryProvider.startController(
    httpPort = None, httpsPort = Some(backupHttpsPort)
  ).await(99.s)

  protected final val standardUserAndPassword: Option[UserAndPassword] =
    (!controllerHttpsMutual || extraDistringuishedNameUserAndPassword.isDefined) ?
      UserAndPassword(UserId("TEST"), SecretString("TEST-PASSWORD"))

  protected lazy val controllerApi = new AkkaHttpControllerApi(
    controller.localUri,
    standardUserAndPassword,
    actorSystem,
    config,
    keyStoreRef = Some(KeyStoreRef(clientKeyStore,
      storePassword = SecretString("jobscheduler"),
      keyPassword = SecretString("jobscheduler"))),
    trustStoreRefs = ExportedControllerTrustStoreRef :: Nil,
  ).closeWithCloser

  override protected final def agentHttps = true
  protected final val agentIds = AgentId("TEST-AGENT") :: Nil
  protected final val versionedItems = TestWorkflow :: Nil

  override def beforeAll() = {
    clientKeyStore := ClientKeyStoreResource.contentBytes
    provideAgentConfiguration(directoryProvider.agents(0))
    provideControllerConfiguration(directoryProvider.controller)
    // We have provided our own certificates: backupDirectoryProvider.controller.provideHttpsCertificates()

    super.beforeAll()
  }

  override def afterAll() = {
    close()
    delete(clientKeyStore)
    super.afterAll()
    if (useCluster) {
      backupController.terminate() await 99.s
      backupDirectoryProvider.close()
    }
  }
}

private[https] object HttpsTestBase
{
  /* Following resources have been generated with the command line:
     js7-common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh \
        --alias=client \
        --distinguished-name="CN=Test client, DC=test-client, DC=HttpsTestBase, DC=tests, DC=js7, DC=sh" \
        --host=localhost \
        --prefix="client-" \
        --config-directory=js7-tests/src/test/resources/js7/tests/https/resources
   */
  private val ClientKeyStoreResource = JavaResource("js7/tests/https/resources/private/client-https-keystore.p12")
  private val ExportedClientTrustStoreResource = JavaResource("js7/tests/https/resources/export/client-https-truststore.p12")
  val ClientKeyStoreRef = KeyStoreRef(ClientKeyStoreResource.url, SecretString("jobscheduler"), SecretString("jobscheduler"))

  /* js7-common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh \
        --alias=backup-controller \
        --distinguished-name="CN=Backup Controller, DC=backup-controller, DC=HttpsTestBase, DC=tests, DC=js7, DC=sh" \
        --host=localhost \
        --prefix="backup-controller-" \
        --config-directory=js7-tests/src/test/resources/js7/tests/https/resources
   */
  private val BackupKeyStoreResource = JavaResource("js7/tests/https/resources/private/backup-controller-https-keystore.p12")
  private val ExportedBackupTrustStoreResource = JavaResource("js7/tests/https/resources/export/backup-controller-https-truststore.p12")

  private val TestWorkflow = WorkflowParser.parse(WorkflowPath("/TEST-WORKFLOW"), s"""
    define workflow {
      execute executable="TEST$sh", agent="TEST-AGENT";
    }""").orThrow

  private def provideAgentConfiguration(agent: DirectoryProvider.AgentTree): Unit =
    agent.writeExecutable(RelativePathExecutable(s"TEST$sh"), operatingSystem.sleepingShellScript(0.seconds))

  private def provideControllerConfiguration(controller: DirectoryProvider.ControllerTree): Unit =
    controller.configDir / "private/private.conf" ++= """
      |js7.auth.users {
      |  TEST-USER: "plain:TEST-PASSWORD"
      |}
      |""".stripMargin
}
