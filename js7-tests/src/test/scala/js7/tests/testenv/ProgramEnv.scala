package js7.tests.testenv

import cats.effect.Resource
import java.io.IOException
import java.nio.file.Files.createDirectory
import java.nio.file.Path
import js7.base.crypt.SignatureVerifier
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.tests.testenv.ProgramEnv.*
import monix.eval.Task

trait ProgramEnv extends AutoCloseable {
  type Program

  def programResource: Resource[Task, Program]

  val directory: Path
  protected def verifier: SignatureVerifier
  protected def suppressSignatureKeys: Boolean = false

  final lazy val configDir = directory / "config"
  final lazy val dataDir = directory / "data"
  final lazy val stateDir = dataDir / "state"
  private lazy val trustedSignatureKeysDir =
    "private/" + verifier.companion.recommendedKeyDirectoryName

  def close() =
    try deleteDirectoryRecursively(directory)
    catch {
      case e: IOException => logger.error(s"Remove $directory => ${e.toStringWithCauses}")
    }

  def journalFileBase: Path

  protected def createDirectoriesAndFiles(): Unit = {
    createDirectory(directory)
    createDirectory(configDir)
    createDirectory(configDir / "private")
    createDirectory(dataDir)
    createDirectory(dataDir / "work")
  }

  //protected final def programConfig: Config =
  //  config"""
  //    js7.configuration.trusted-signature-keys {
  //      ${verifier.companion.typeName} = $${js7.config-directory}"/$trustedSignatureKeysDir"
  //    }"""

  protected final def writeTrustedSignatureKeys(confFilename: String): Unit = {
    createDirectory(configDir / trustedSignatureKeysDir)
    if (!suppressSignatureKeys) {
      for ((key, i) <- verifier.publicKeys.zipWithIndex) {
        val file = configDir / trustedSignatureKeysDir /
          s"key-${i + 1}${verifier.companion.filenameExtension}"
        logger.trace(s"$file := key")
        file := key
      }
    }

    configDir / confFilename ++=
      s"""js7.configuration.trusted-signature-keys {
         |  ${verifier.companion.typeName} = $${js7.config-directory}"/$trustedSignatureKeysDir"
         |}
         |""".stripMargin
  }
}

object ProgramEnv {
  private val logger = Logger[this.type]
}
