package com.sos.scheduler.engine.agent.test

import com.sos.scheduler.engine.agent.test.AgentConfigDirectoryProvider.{HttpJksResource, PasswordsResource}
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.scalautil.AutoClosing.closeOnError
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.sprayutils.https.KeystoreReference
import com.sos.scheduler.engine.common.utils.JavaResource
import java.nio.file.Files.{createDirectories, createTempDirectory, delete}
import java.nio.file.Path

trait AgentConfigDirectoryProvider {
  this: HasCloser ⇒

  final lazy val dataDirectory = createTempDirectory("TextAgentClientIT-") withCloser delete
  private lazy val keystoreJksFile: Path = dataDirectory / "config" / "private" / "https.jks"
  final lazy val keystoreReference = KeystoreReference(
    keystoreJksFile.toURI.toURL,
    keyPassword = SecretString("jobscheduler"),
    storePassword = Some(SecretString("jobscheduler")))
  private val privateDir = createDirectories(dataDirectory / "config" / "private")

  closeOnError(closer) {
    dataDirectory
    onClose {
      delete(privateDir)
      delete(dataDirectory / "config")
    }
    HttpJksResource.copyToFile(keystoreJksFile) withCloser delete
    PasswordsResource.copyToFile(privateDir / "passwords.conf") withCloser delete
  }
}

object AgentConfigDirectoryProvider {
  val HttpJksResource = JavaResource("com/sos/scheduler/engine/agent/test/config/private/https.jks")
  val PasswordsResource = JavaResource("com/sos/scheduler/engine/agent/test/config/private/passwords.conf")
}
