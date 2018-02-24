package com.sos.jobscheduler.agent.test

import com.sos.jobscheduler.agent.test.TestAgentDirectoryProvider._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.https.KeystoreReference
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
      val privateDir = createDirectories(agentDirectory / "config/private")
      PrivateHttpJksResource.copyToFile(agentDirectory / KeystoreJksLocation) withCloser delete
      PrivateConfResource.copyToFile(privateDir / "private.conf") withCloser delete
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

  final lazy val keystoreReference = KeystoreReference(
    (agentDirectory / KeystoreJksLocation).toURI.toURL,
    storePassword = Some(SecretString("jobscheduler")),
    keyPassword = Some(SecretString("jobscheduler")))
}

object TestAgentDirectoryProvider {
  // Following resources have been generated with the command line:
  // common/src/main/resources/com/sos/jobscheduler/common/http/https/generate-self-signed-ssl-certificate-test-keystore.sh -data-directory=engine-agent/src/main/resources/com/sos/jobscheduler/agent/test -alias=agent-https
  val PrivateHttpJksResource = JavaResource("com/sos/jobscheduler/agent/test/config/private/private-https.jks")
  val PublicHttpJksResource = JavaResource("com/sos/jobscheduler/agent/test/config/public-https.jks")
  val PrivateConfResource = JavaResource("com/sos/jobscheduler/agent/test/config/private/private.conf")
  private val KeystoreJksLocation = "config/private/private-https.jks"
  private val logger = Logger(getClass)

  def provideAgentDirectory[A](body: Path ⇒ A): A =
    autoClosing(new TestAgentDirectoryProvider {}) { provider ⇒
      body(provider.agentDirectory)
    }
}
