package js7.tests.testenv

import cats.effect.Resource
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Path
import js7.base.crypt.SignatureVerifier
import js7.base.eventbus.StandardEventBus
import js7.base.io.file.FileUtils.syntax.*
import js7.common.system.ThreadPools.ownThreadPoolResource
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.subagent.Subagent
import js7.tests.testenv.DirectoryProvider.*
import monix.eval.Task

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
extends SubagentEnv {
  type Program = Subagent

  initialize()

  protected override def createDirectoriesAndFiles(): Unit = {
    super.createDirectoriesAndFiles()

    privateConf ++= s"""
     |js7.auth.users.${directorSubagentId.string} {
     |  permissions: [ AgentDirector ]
     |  password: "plain:${directorSubagentId.string}'s PASSWORD"
     |}
     |""".stripMargin
  }

  def programResource: Resource[Task, Subagent] =
    subagentResource

  def subagentResource: Resource[Task, Subagent] =
    ownThreadPoolResource(subagentConf.name, subagentConf.config)(implicit scheduler =>
      Subagent.resource(subagentConf, new StandardEventBus[Any]))

  override def toString = s"BareSubagentEnv($name)"
}
