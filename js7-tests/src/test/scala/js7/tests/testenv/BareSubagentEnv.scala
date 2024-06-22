package js7.tests.testenv

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, ResourceIO}
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.base.catsutils.OurIORuntime
import js7.base.crypt.SignatureVerifier
import js7.base.eventbus.StandardEventBus
import js7.base.io.file.FileUtils.syntax.*
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.subagent.Subagent
import js7.tests.testenv.DirectoryProvider.*

/** Environment with config and data directories for a bare Subagent. */
final class BareSubagentEnv private[testenv](
  val subagentItem: SubagentItem,
  directorSubagentId: SubagentId,
  protected val name: String,
  protected val rootDirectory: Path,
  protected val verifier: SignatureVerifier = defaultVerifier,
  protected val mutualHttps: Boolean = false,
  protected val provideHttpsCertificate: Boolean = false,
  protected val provideClientCertificate: Boolean = false,
  override protected val suppressSignatureKeys: Boolean = false,
  protected val extraConfig: Config = ConfigFactory.empty)
extends SubagentEnv:
  type Program = Subagent

  initialize()

  protected override def createDirectoriesAndFiles(): Unit =
    super.createDirectoriesAndFiles()

    privateConf ++= s"""
     |js7.auth.users.${directorSubagentId.string} {
     |  permissions: [ AgentDirector ]
     |  password: "plain:${directorSubagentId.string}'s PASSWORD"
     |}
     |""".stripMargin

  def programResource(using IORuntime): ResourceIO[Subagent] =
    subagentResource

  def subagentResource: ResourceIO[Subagent] =
    for
      given IORuntime <- OurIORuntime.resource[IO](subagentConf.name, subagentConf.config)
      subagent <- Subagent
        .resource(subagentConf, new StandardEventBus)
        .evalOn(given_IORuntime.compute)
    yield
      subagent

  override def toString = s"BareSubagentEnv($name)"
