package js7.tests.testenv

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.option.*
import com.typesafe.config.{Config, ConfigFactory}
import java.io.IOException
import java.nio.file.Files.createDirectory
import java.nio.file.Path
import js7.base.crypt.SignatureVerifier
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.utils.AutoClosing.closeOnError
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.common.configuration.Js7Configuration
import js7.data.event.SnapshotableState
import js7.journal.data.JournalLocation
import js7.journal.recover.StateRecoverer
import js7.tests.testenv.ProgramEnv.*

trait ProgramEnv extends AutoCloseable:
  type Program

  val directory: Path
  protected def confFilename: String
  def programResource(using IORuntime): ResourceIO[Program]
  protected def verifier: SignatureVerifier
  protected def suppressSignatureKeys: Boolean = false

  protected def onInitialize() = {}

  final lazy val configDir = directory / "config"
  final lazy val dataDir = directory / "data"
  final lazy val stateDir = dataDir / "state"
  final lazy val privateConf = configDir / "private" / "private.conf"
  private var _currentProgram = none[Program]

  private lazy val trustedSignatureKeysDir =
    "private/" + verifier.companion.recommendedKeyDirectoryName

  def close() =
    try deleteDirectoryRecursively(directory)
    catch
      case e: IOException => logger.error(s"Remove $directory => ${e.toStringWithCauses}")

  protected def ownConfig: Config =
    ConfigFactory.empty

  protected def createDirectoriesAndFiles(): Unit =
    createDirectories()
    writeTrustedSignatureKeys()

  private def createDirectories() =
    createDirectory(directory)
    createDirectory(configDir)
    createDirectory(configDir / "private")
    createDirectory(dataDir)
    createDirectory(dataDir / "work")

  private def writeTrustedSignatureKeys() =
    createDirectory(configDir / trustedSignatureKeysDir)

    if !suppressSignatureKeys then
      for (key, i) <- verifier.publicKeys.zipWithIndex do
        val file = configDir / trustedSignatureKeysDir /
          s"key-${i + 1}${verifier.companion.filenameExtension}"
        logger.trace(s"$file := key")
        file := key

    configDir / confFilename ++= s"""
     |js7.configuration.trusted-signature-keys {
     |  ${verifier.companion.typeName} = $${js7.config-directory}"/$trustedSignatureKeysDir"
     |}
     |""".stripMargin

  protected def initialize(): Unit =
    closeOnError(this):
      createDirectoriesAndFiles()
      onInitialize()

  protected final def programRegistering(program: Program): ResourceIO[Program] =
    Resource.make(
      acquire = IO {
        if _currentProgram.isDefined then throw new IllegalStateException(
          s"$toString: Second program start")
        _currentProgram = Some(program)
        program
      })(release =
      _ => IO {
        _currentProgram = None
      })

  def program(): Program =
    _currentProgram.getOrElse(throw new IllegalStateException(s"$toString: Program not started"))


object ProgramEnv:
  private val logger = Logger[this.type]

  trait WithFileJournal extends ProgramEnv:
    protected type S <: SnapshotableState[S]

    implicit val S: SnapshotableState.Companion[S]

    def journalLocation: JournalLocation

    def recoverState(using IORuntime): S =
      StateRecoverer
        .recover[S](journalLocation, Js7Configuration.defaultConfig)
        .state
