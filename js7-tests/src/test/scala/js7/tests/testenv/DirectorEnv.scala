package js7.tests.testenv

import cats.effect
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, ResourceIO}
import com.typesafe.config.ConfigUtil.quoteString
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.{RestartableDirector, RunningAgent}
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.catsutils.OurIORuntime
import js7.base.configutils.Configs.{HoconStringInterpolator, configIf}
import js7.base.crypt.SignatureVerifier
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.journal.data.JournalLocation
import js7.tests.testenv.DirectoryProvider.*

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
extends SubagentEnv, ProgramEnv.WithFileJournal:

  type Program = RunningAgent
  protected type S = AgentState
  val S = AgentState

  final val journalLocation = JournalLocation(AgentState, stateDir / "agent")

  override protected def internalSubagentConfig =
    AgentConfiguration.DefaultConfig

  override protected lazy val ownConfig =
    config"""
      js7.web.server.auth.https-client-authentication = $mutualHttps
      js7.web.https.keystore {
        store-password = "jobscheduler"
        key-password = "jobscheduler"
      }"""
      .withFallback(
        configIf(isClusterBackup,
          config"""js7.journal.cluster.node.is-backup = yes"""))
      .withFallback(super.ownConfig)

  final val controllerPassword = SecretString(s"$agentPath-PASSWORD")
  final val controllerUserAndPassword =
    Some(UserAndPassword(UserId("Controller"), controllerPassword))

  lazy val agentConf: AgentConfiguration =
    AgentConfiguration.fromDirectories(subagentConf, name = name)

  initialize()

  protected override def createDirectoriesAndFiles() =
    super.createDirectoriesAndFiles()

    privateConf ++= s"""
     |js7.auth.users {
     |  Controller {
     |    password = ${quoteString("plain:" + controllerPassword.string)}
     |    distinguished-names = [
     |      "CN=Primary Controller, DC=primary-controller, DC=DirectoryProvider, DC=tests, DC=js7, DC=sh",
     |      "CN=Backup Controller,DC=backup-controller,DC=HttpsTestBase,DC=tests,DC=js7,DC=sh"
     |    ]
     |  }
     |}
     |js7.web.server.auth.https-client-authentication = $mutualHttps
     |js7.web.https.keystore {
     |  store-password = "jobscheduler"
     |  key-password = "jobscheduler"
     |}
     |""".stripMargin

    for otherSubagentId <- otherSubagentIds.toList do
      privateConf ++= s"""
       |js7.auth.subagents.${otherSubagentId.string} = "${subagentItem.id.string}'s PASSWORD"
       |js7.auth.users.${otherSubagentId.string} {
       |  permissions = [ AgentDirector ]
       |  password = "plain:${otherSubagentId.string}'s PASSWORD"
       |}
       |""".stripMargin

  def programResource(using IORuntime): ResourceIO[RunningAgent] =
    directorResource

  def directorResource: ResourceIO[RunningAgent] =
    for
      given IORuntime <- ioRuntimeResource
      agent <- RunningAgent.resource(agentConf)
      _ <- programRegistering(agent)
    yield
      agent

  def restartableDirectorResource: ResourceIO[RestartableDirector] =
    for
      given IORuntime <- ioRuntimeResource
      agent <- RunningAgent.restartable(agentConf)
    yield
      agent

  private def ioRuntimeResource: ResourceIO[IORuntime] =
    Resource.suspend/*delay access to agentConf*/(IO:
      OurIORuntime.resource[IO](agentConf.name, agentConf.config))

  override def toString = s"DirectorEnv($name)"
