package com.sos.jobscheduler.agent.tests

import com.sos.jobscheduler.agent.tests.TestAgentDirectoryProvider._
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.base.utils.Closer.syntax.RichClosersAny
import com.sos.jobscheduler.base.utils.HasCloser
import com.sos.jobscheduler.common.scalautil.FileUtils._
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.Exceptions.repeatUntilNoException
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.core.crypt.silly.{SillySignature, SillySigner}
import com.sos.jobscheduler.core.filebased.FileBasedSigner
import com.sos.jobscheduler.data.workflow.Workflow
import java.nio.file.Files.{createDirectories, createDirectory, createTempDirectory, delete}
import java.nio.file.Path
import scala.util.control.NonFatal

trait TestAgentDirectoryProvider extends HasCloser
{
  private val signature = SillySignature("MY-SILLY-SIGNATURE")
  final val fileBasedSigner = new FileBasedSigner(new SillySigner(signature), Workflow.jsonEncoder)

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
      s"""jobscheduler.https.keystore {
         |  store-password = "jobscheduler"
         |  key-password = "jobscheduler"
         |}
         |""".stripMargin)
  }

  private def provideSignature(configDirectory: Path): Unit = {
    configDirectory / "private" / "trusted-silly-signature-key.txt" := signature.string
    configDirectory / "private" / "private.conf" ++=
      s"""|jobscheduler.configuration.trusted-signature-keys.Silly = $${jobscheduler.config-directory}"/private/trusted-silly-signature-key.txt"
         |""".stripMargin
  }
}

object TestAgentDirectoryProvider
{
  def apply() = new TestAgentDirectoryProvider {}

  /* Following resources have been generated with the command lines:
     common/src/main/resources/com/sos/jobscheduler/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=agent-https -config-directory=agent/src/test/resources/com/sos/jobscheduler/agent/test/config

  */
  private val PrivateHttpJksResource = JavaResource("com/sos/jobscheduler/agent/tests/config/private/https-keystore.p12")
  private val PrivateConfResource = JavaResource("com/sos/jobscheduler/agent/tests/config/private/private.conf")
  val TestUserAndPassword = UserAndPassword(UserId("SHA512-USER"), SecretString("SHA512-PASSWORD"))
  private val logger = Logger(getClass)

  def provideAgentDirectory[A](body: Path => A): A =
    autoClosing(TestAgentDirectoryProvider()) { provider =>
      body(provider.agentDirectory)
    }
}
