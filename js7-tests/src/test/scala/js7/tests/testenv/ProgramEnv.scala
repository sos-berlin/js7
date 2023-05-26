package js7.tests.testenv

import java.nio.file.Files.createDirectory
import java.nio.file.Path
import js7.base.crypt.SignatureVerifier
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.tests.testenv.ProgramEnv.*

trait ProgramEnv {
  val directory: Path
  protected def verifier: SignatureVerifier
  protected def suppressSignatureKeys: Boolean = false

  lazy val configDir = directory / "config"
  lazy val dataDir = directory / "data"
  lazy val stateDir = dataDir / "state"
  private val trustedSignatureKeysDir =
    "private/" + verifier.companion.recommendedKeyDirectoryName

  def journalFileBase: Path

  private[testenv] def createDirectoriesAndFiles(): Unit = {
    createDirectory(directory)
    createDirectory(configDir)
    createDirectory(configDir / "private")
    createDirectory(dataDir)
    createDirectory(dataDir / "work")
  }

  protected final def writeTrustedSignatureKeys(confFilename: String): Unit = {
    createDirectory(configDir / trustedSignatureKeysDir)
    if (!suppressSignatureKeys) {
      for ((key, i) <- verifier.publicKeys.zipWithIndex) {
        val file = configDir / trustedSignatureKeysDir / (s"key-${i + 1}${verifier.companion.filenameExtension}")
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
