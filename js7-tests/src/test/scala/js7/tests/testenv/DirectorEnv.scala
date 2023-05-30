package js7.tests.testenv

import cats.effect.Resource
import cats.syntax.foldable.*
import com.typesafe.config.ConfigUtil.quoteString
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.configutils.Configs.{HoconStringInterpolator, configIf, configMonoid}
import js7.base.crypt.SignatureVerifier
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.monixutils.MonixBase.syntax.RichMonixResource
import js7.common.system.ThreadPools.ownThreadPoolResource
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.tests.testenv.DirectoryProvider.*
import monix.eval.Task

/** Environment with config and data directories for a Subagent with Agent Director. */
final class DirectorEnv(
  val subagentItem: SubagentItem,
  protected val name: String,
  protected val rootDirectory: Path,
  protected val verifier: SignatureVerifier = defaultVerifier,
  protected val mutualHttps: Boolean = false,
  protected val provideHttpsCertificate: Boolean = false,
  protected val provideClientCertificate: Boolean = false,
  protected val isClusterBackup: Boolean = false,
  override protected val suppressSignatureKeys: Boolean = false,
  protected val otherSubagentIds: Seq[SubagentId] = Nil,
  protected val extraConfig: Config = ConfigFactory.empty)
extends SubagentEnv {
  type Program = RunningAgent

  private val clusterUserAndPassword =
    UserAndPassword(UserId("Agent"), SecretString("AGENT-PASSWORD"))

  override protected def internalSubagentConfig =
    AgentConfiguration.DefaultConfig

  override protected def ownConfig =
    config"""
      js7.web.server.auth.https-client-authentication = $mutualHttps
      js7.web.https.keystore {
        store-password = "jobscheduler"
        key-password = "jobscheduler"
      }"""
      .withFallback(
        configIf(isClusterBackup,
          config"""js7.journal.cluster.node.is-backup = yes"""))
      .withFallback(otherSubagentIds
        .map(subagentId => config"""
          js7.auth.subagents.${subagentId.string} = "AGENT-PASSWORD"
         """)
        .combineAll)
      .withFallback(super.ownConfig)

  final val controllerPassword = SecretString(s"$agentPath-PASSWORD") // TODO AgentPath — or SubagentId?
  final val controllerUserAndPassword =
    Some(UserAndPassword(UserId("Controller"), controllerPassword))

  lazy val agentConf: AgentConfiguration =
    AgentConfiguration.fromDirectories(subagentConf, name = name)

  initialize()

  protected override def createDirectoriesAndFiles(): Unit = {
    super.createDirectoriesAndFiles()
    configDir / "private" / "private.conf" ++= s"""
     |js7.auth.users {
     |  Controller {
     |    password = ${quoteString("plain:" + controllerPassword.string)}
     |    distinguished-names = [
     |      "CN=Primary Controller, DC=primary-controller, DC=DirectoryProvider, DC=tests, DC=js7, DC=sh",
     |      "CN=Backup Controller,DC=backup-controller,DC=HttpsTestBase,DC=tests,DC=js7,DC=sh"
     |    ]
     |  }
     |  # Login for Agent cluster:
     |  ${clusterUserAndPassword.userId.string} {
     |    password = "plain:${clusterUserAndPassword.password.string}"
     |  }
     |}
     |js7.auth.cluster.password = "${clusterUserAndPassword.password.string}"
     |""".stripMargin
  }

  def programResource: Resource[Task, RunningAgent] =
    directorResource

  def directorResource: Resource[Task, RunningAgent] =
    ownThreadPoolResource(agentConf.name, agentConf.config)(scheduler =>
      RunningAgent
        .resource(agentConf)(scheduler)
        .executeOn(scheduler))
}
