package js7.tests.testenv

import com.typesafe.config.Config
import java.nio.file.Files.createDirectory
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.RelativePathExecutable
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.subagent.configuration.SubagentConf
import js7.tests.testenv.DirectoryProvider.*

/** Environment with config and data directories for a Subagent with Agent Director. */
trait SubagentEnv extends ProgramEnv {
  protected def subagentItem: SubagentItem
  protected def name: String
  protected def rootDirectory: Path
  protected def mutualHttps: Boolean
  protected def provideHttpsCertificate: Boolean
  protected def provideClientCertificate: Boolean
  protected def extraConfig: Config

  protected def confFilename = "agent.conf"

  final val agentPath = subagentItem.agentPath
  final val directory = rootDirectory / "subagents" / name
  final val localUri = subagentItem.uri

  private lazy val executables = configDir / "executables"

  protected def internalSubagentConfig: Config =
    SubagentConf.DefaultConfig

  final lazy val subagentConf: SubagentConf = {
    val isHttps = subagentItem.uri.string.startsWith("https:")
    val port = subagentItem.uri.port.orThrow

    SubagentConf.forTest(
      directory,
      name = name,
      extraConfig = extraConfig.withFallback(ownConfig),
      internalConfig = internalSubagentConfig,
      httpPort = !isHttps ? port,
      httpsPort = isHttps ? port)
  }

  final def localSubagentId: SubagentId =
    subagentItem.id

  protected override def createDirectoriesAndFiles(): Unit = {
    super.createDirectoriesAndFiles()
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
     |  ${agentPath.string} {
     |    permissions: [ AgentDirector ]
     |    password: "plain:AGENT-PASSWORD"
     |  }
     |}
     |js7.web.server.auth.https-client-authentication = $mutualHttps
     |js7.web.https.keystore {
     |  store-password = "jobscheduler"
     |  key-password = "jobscheduler"
     |}
     |""".stripMargin
  }

  override def onInitialize(): Unit = {
    super.onInitialize()
    // Call finishAndProvideFiles _after_ *.conf files have been written!
    subagentConf.finishAndProvideFiles()
  }

  final def writeExecutable(path: RelativePathExecutable, string: String): Unit =
    path.toFile(executables).writeUtf8Executable(string)
}
