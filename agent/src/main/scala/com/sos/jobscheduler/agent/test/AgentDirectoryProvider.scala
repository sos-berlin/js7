package com.sos.jobscheduler.agent.test

import com.sos.jobscheduler.agent.test.AgentDirectoryProvider._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.scalautil.AutoClosing.{autoClosing, closeOnError}
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.jobscheduler.common.scalautil.FileUtils._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.sprayutils.https.KeystoreReference
import com.sos.jobscheduler.common.utils.JavaResource
import java.nio.file.Files.{createDirectories, createDirectory, createTempDirectory, delete}
import java.nio.file.Path

trait AgentDirectoryProvider extends HasCloser {

  final lazy val agentDirectory = createTempDirectory("AgentDirectoryProvider-") withCloser deleteDirectoryRecursively
  private lazy val keystoreJksFile: Path = agentDirectory / "config/private/private-https.jks"
  final lazy val keystoreReference = KeystoreReference(
    keystoreJksFile.toURI.toURL,
    storePassword = Some(SecretString("jobscheduler")),
    keyPassword = Some(SecretString("jobscheduler")))

  closeOnError(closer) {
    agentDirectory
    val privateDir = createDirectories(agentDirectory / "config/private")
    PrivateHttpJksResource.copyToFile(keystoreJksFile) withCloser delete
    PrivateConfResource.copyToFile(privateDir / "private.conf") withCloser delete
  }
}

object AgentDirectoryProvider {
  // Following resources have been generated with the command line:
  // common/src/main/resources/com/sos/jobscheduler/common/sprayutils/https/generate-self-signed-ssl-certificate-test-keystore.sh -data-directory=engine-agent/src/main/resources/com/sos/jobscheduler/agent/test -alias=agent-https
  val PrivateHttpJksResource = JavaResource("com/sos/jobscheduler/agent/test/config/private/private-https.jks")
  val PublicHttpJksResource = JavaResource("com/sos/jobscheduler/agent/test/public-https.jks")
  val PrivateConfResource = JavaResource("com/sos/jobscheduler/agent/test/config/private/private.conf")

  def provideAgent2Directory[A](body: Path ⇒ A): A =
    autoClosing(new AgentDirectoryProvider {}) { provider ⇒
      createDirectory(provider.agentDirectory / "config" / "live")
      createDirectory(provider.agentDirectory / "data")
      body(provider.agentDirectory)
    }
}
