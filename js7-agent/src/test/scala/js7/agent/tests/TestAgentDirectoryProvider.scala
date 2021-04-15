package js7.agent.tests

import java.nio.file.Files.{createDirectories, createDirectory, createTempDirectory, delete}
import java.nio.file.Path
import js7.agent.data.AgentState
import js7.agent.tests.TestAgentDirectoryProvider._
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.crypt.silly.{SillySignature, SillySigner}
import js7.base.generic.SecretString
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils._
import js7.base.io.file.FileUtils.syntax._
import js7.base.log.Logger
import js7.base.log.ScribeUtils.coupleScribeWithSlf4j
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer.syntax.RichClosersAny
import js7.base.utils.HasCloser
import js7.common.message.ProblemCodeMessages
import js7.common.utils.Exceptions.repeatUntilNoException
import js7.data.item.VersionedItemSigner
import scala.util.control.NonFatal

trait TestAgentDirectoryProvider extends HasCloser
{
  ProblemCodeMessages.initialize()
  coupleScribeWithSlf4j()

  private val signature = SillySignature("MY-SILLY-SIGNATURE")
  final val itemSigner = new VersionedItemSigner(new SillySigner(signature), AgentState.versionedItemJsonCodec)

  final lazy val agentDirectory = {
    val agentDirectory = createTempDirectory("TestAgentDirectoryProvider-") withCloser { dir =>
      logger.debug(s"Deleting $dir")
      repeatUntilNoException(9.s, 10.ms) {  // For Windows
        deleteDirectoryRecursively(dir)
      }
    }
    try {
      createDirectories(agentDirectory / "config/private")
      PrivateConfResource.copyToFile(agentDirectory / "config/private/private.conf") withCloser delete
      provideSignature(agentDirectory / "config")
    } catch { case NonFatal(t) =>
      repeatUntilNoException(9.s, 10.ms) {  // For Windows
        deleteDirectoryRecursively(agentDirectory)
      }
      throw t
    }
    createDirectory(agentDirectory / "config" / "private" / "trusted-pgp-keys")
    createDirectory(agentDirectory / "config" / "executables")
    createDirectory(agentDirectory / "data")
    agentDirectory
  }
  final lazy val configDirectory = agentDirectory / "config"
  final lazy val dataDirectory = agentDirectory / "data"

  protected[agent] def provideHttpsFiles(): Unit = {
    // Certificate files are under src/test/resources and only available for module "agent".
    PrivateHttpJksResource.copyToFile(agentDirectory / "config/private/https-keystore.p12") withCloser delete
    (agentDirectory / "config/private/private.conf").append(
      s"""js7.web.https.keystore {
         |  store-password = "jobscheduler"
         |  key-password = "jobscheduler"
         |}
         |""".stripMargin)
  }

  private def provideSignature(configDirectory: Path): Unit = {
    val directory = configDirectory / "private" / "trusted-silly-signature-keys"
    createDirectory(directory)
    directory / "trusted-silly-signature-key.txt" := signature.string
    configDirectory / "private" / "private.conf" ++=
      s"""|js7.configuration.trusted-signature-keys.Silly = $${js7.config-directory}"/private/trusted-silly-signature-keys"
          |""".stripMargin
  }
}

object TestAgentDirectoryProvider
{
  def apply() = new TestAgentDirectoryProvider {}

  /* Following resources have been generated with the command lines:
     js7-common/src/main/resources/js7/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh \
        --distinguished-name="CN=Web server, DC=webserver, DC=TestAgentDirectoryProvider, DC=tests, DC=js7, DC=sh" \
        --alias=webserver \
        --host=localhost \
        --config-directory=js7-agent/src/test/resources/js7/agent/tests/config
  */
  private val PrivateHttpJksResource = JavaResource("js7/agent/tests/config/private/https-keystore.p12")
  private val PrivateConfResource = JavaResource("js7/agent/tests/config/private/private.conf")
  val TestUserAndPassword = UserAndPassword(UserId("SHA512-USER"), SecretString("SHA512-PASSWORD"))
  private val logger = Logger(getClass)

  def provideAgentDirectory[A](body: Path => A): A =
    autoClosing(TestAgentDirectoryProvider()) { provider =>
      body(provider.agentDirectory)
    }
}
