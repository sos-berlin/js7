package com.sos.jobscheduler.agent.test

import com.sos.jobscheduler.agent.test.TestAgentDirectoryProvider._
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.jobscheduler.common.scalautil.FileUtils._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger}
import com.sos.jobscheduler.common.utils.JavaResource
import java.nio.file.Files.{createDirectories, createDirectory, createTempDirectory, delete}
import java.nio.file.Path
import scala.util.control.NonFatal

trait TestAgentDirectoryProvider extends HasCloser {

  final lazy val agentDirectory = {
    val agentDirectory = createTempDirectory("TestAgentDirectoryProvider-") withCloser { dir ⇒
      logger.debug(s"Deleting $dir")
      deleteDirectoryRecursively(dir)
    }
    try {
      createDirectories(agentDirectory / "config/private")
      PrivateConfResource.copyToFile(agentDirectory / "config/private/private.conf") withCloser delete
    } catch { case NonFatal(t) ⇒
      deleteDirectoryRecursively(agentDirectory)
      throw t
    }
    createDirectory(agentDirectory / "config" / "live")
    createDirectory(agentDirectory / "data")
    agentDirectory
  }
  final lazy val configDirectory = agentDirectory / "config"
  final lazy val dataDirectory = agentDirectory / "data"

  protected[agent] def provideHttpsFiles(): Unit = {
    // Certificate files are under src/test/resources and only available for module "agent".
    PrivateHttpJksResource.copyToFile(agentDirectory / "config/private/private-https.jks") withCloser delete
    PublicHttpJksResource.copyToFile(agentDirectory / "config/public-https.jks") withCloser delete
  }
}

object TestAgentDirectoryProvider {
  // Following resources have been generated with the command line:
  // common/src/main/resources/com/sos/jobscheduler/common/akkahttp/https/generate-self-signed-ssl-certificate-test-keystore.sh -host=localhost -alias=agent-https -config-directory=agent/src/test/resources/com/sos/jobscheduler/agent/test/config
  private val PrivateHttpJksResource = JavaResource("com/sos/jobscheduler/agent/test/config/private/private-https.jks")
  private val PublicHttpJksResource = JavaResource("com/sos/jobscheduler/agent/test/config/public-https.jks")
  private val PrivateConfResource = JavaResource("com/sos/jobscheduler/agent/test/config/private/private.conf")
  val TestUserAndPassword = UserAndPassword(UserId("SHA512-USER"), SecretString("SHA512-PASSWORD"))
  private val logger = Logger(getClass)

  def provideAgentDirectory[A](body: Path ⇒ A): A =
    autoClosing(new TestAgentDirectoryProvider {}) { provider ⇒
      body(provider.agentDirectory)
    }
}
