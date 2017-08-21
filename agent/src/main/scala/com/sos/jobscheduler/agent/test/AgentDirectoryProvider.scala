package com.sos.jobscheduler.agent.test

import com.sos.jobscheduler.agent.test.AgentDirectoryProvider._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.jobscheduler.common.scalautil.FileUtils._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.sprayutils.https.KeystoreReference
import com.sos.jobscheduler.common.utils.JavaResource
import java.nio.file.Files.{createDirectories, createDirectory, createTempDirectory, delete}
import java.nio.file.Path
import scala.util.control.NonFatal

trait AgentDirectoryProvider extends HasCloser {

  final lazy val agentDirectory = {
    val agentDirectory = createTempDirectory("AgentDirectoryProvider-") withCloser deleteDirectoryRecursively
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

object AgentDirectoryProvider {
  // Following resources have been generated with the command line:
  // common/src/main/resources/com/sos/jobscheduler/common/sprayutils/https/generate-self-signed-ssl-certificate-test-keystore.sh -data-directory=engine-agent/src/main/resources/com/sos/jobscheduler/agent/test -alias=agent-https
  val PrivateHttpJksResource = JavaResource("com/sos/jobscheduler/agent/test/config/private/private-https.jks")
  val PublicHttpJksResource = JavaResource("com/sos/jobscheduler/agent/test/public-https.jks")
  val PrivateConfResource = JavaResource("com/sos/jobscheduler/agent/test/config/private/private.conf")
  private val KeystoreJksLocation = "config/private/private-https.jks"

  def provideAgent2Directory[A](body: Path ⇒ A): A =
    autoClosing(new AgentDirectoryProvider {}) { provider ⇒
      body(provider.agentDirectory)
    }
}
