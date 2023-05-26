package js7.tests.testenv

import cats.syntax.foldable.*
import com.typesafe.config.ConfigUtil.quoteString
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files.createDirectory
import java.nio.file.Path
import js7.agent.configuration.AgentConfiguration
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.configutils.Configs.{HoconStringInterpolator, *}
import js7.base.crypt.SignatureVerifier
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.*
import js7.base.utils.CatsUtils.combine
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.job.RelativePathExecutable
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.subagent.configuration.SubagentConf
import js7.tests.testenv.DirectoryProvider.*

/** Environment with config and data directories for a Subagent. */
final class SubagentEnv(
  subagentItem: SubagentItem,
  name: String,
  rootDirectory: Path,
  protected val verifier: SignatureVerifier = defaultVerifier,
  mutualHttps: Boolean = false,
  provideHttpsCertificate: Boolean = false,
  provideClientCertificate: Boolean = false,
  bareSubagentIds: Seq[SubagentId] = Nil,
  subagentsDisabled: Boolean = false,
  isClusterBackup: Boolean = false,
  override protected val suppressSignatureKeys: Boolean = false,
  config: Config = ConfigFactory.empty)
extends ProgramEnv {
  val agentPath = subagentItem.agentPath
  val directory = rootDirectory / "subagents" / name
  val journalFileBase = stateDir / "agent"

  val localUri = subagentItem.uri
  private val port = subagentItem.uri.port.orThrow
  private val https = subagentItem.uri.string.startsWith("https:")

  lazy val agentConf: AgentConfiguration =
    AgentConfiguration.forTest(directory,
      name = name,
      config
        .withFallback(
          combine(
            configIf(isClusterBackup,
              config"""js7.journal.cluster.node.is-backup = yes"""),
            config"""
              js7.auth.users.${agentPath.string} {
                permissions: [ AgentDirector ]
                password: "plain:AGENT-PASSWORD"
              }
              """))
        .withFallback(bareSubagentIds
          .map(subagentId => config"""js7.auth.subagents.${subagentId.string} = "AGENT-PASSWORD" """)
          .combineAll),
      httpPort = !https ? port,
      httpsPort = https ? port)

  lazy val password = SecretString(s"$agentPath-PASSWORD") // TODO AgentPath — or SubagentId?
  lazy val userAndPassword = Some(UserAndPassword(UserId("Controller"), password))
  lazy val executables = configDir / "executables"
  lazy val bareSubagentItems =
    for (subagentId <- bareSubagentIds) yield
      SubagentItem(
        subagentId, agentPath, findFreeLocalUri(),
        disabled = subagentsDisabled)
  lazy val subagentItems = subagentItem +: bareSubagentItems

  def delete(): Unit =
    deleteDirectoryRecursively(directory)

  def localSubagentId: SubagentId =
    subagentItem.id

  override def createDirectoriesAndFiles(): Unit = {
    super.createDirectoriesAndFiles()
    //createDirectory(trustedSignatureDir)
    createDirectory(executables)
    if (provideHttpsCertificate) {
      (configDir / "private/https-keystore.p12") := AgentKeyStoreResource.contentBytes
      if (provideClientCertificate) {
        configDir / "private/controller-https-truststore.p12" :=
          ExportedControllerTrustStoreResource.contentBytes
        configDir / "private/private.conf" ++= s"""
         |js7.web.https.truststores = [
         |  {
         |    file = $${js7.config-directory}/private/controller-https-truststore.p12
         |    store-password = "jobscheduler"
         |  }
         |]
         |""".stripMargin
      }
    }
    configDir / "private" / "private.conf" ++= s"""
     |js7.auth.users {
     |  Controller {
     |    password = ${quoteString("plain:" + password.string)}
     |    distinguished-names = [
     |      "CN=Primary Controller, DC=primary-controller, DC=DirectoryProvider, DC=tests, DC=js7, DC=sh",
     |      "CN=Backup Controller,DC=backup-controller,DC=HttpsTestBase,DC=tests,DC=js7,DC=sh"
     |    ]
     |  }
     |  Agent {
     |    password = "plain:AGENT-PASSWORD"
     |  }
     |}
     |js7.auth.cluster.password = "AGENT-PASSWORD"
     |js7.web.server.auth.https-client-authentication = $mutualHttps
     |js7.web.https.keystore {
     |  store-password = "jobscheduler"
     |  key-password = "jobscheduler"
     |}
     |""".stripMargin

    writeTrustedSignatureKeys("agent.conf")
  }

  def writeExecutable(path: RelativePathExecutable, string: String): Unit =
    path.toFile(executables).writeUtf8Executable(string)

  lazy val subagentConf: SubagentConf =
    agentConf.subagentConf
}
