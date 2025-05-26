package js7.base.crypt.generic

import cats.effect
import cats.effect.{IO, ResourceIO}
import cats.instances.either.*
import cats.instances.vector.*
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import java.nio.file.Files.exists
import java.nio.file.{Path, Paths}
import js7.base.Problems.UnknownSignatureTypeProblem
import js7.base.catsutils.CatsEffectExtensions.catchAsChecked
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier.{State, logger}
import js7.base.crypt.{GenericSignature, SignatureVerifier, SignerId}
import js7.base.data.ByteArray
import js7.base.fs2utils.StreamExtensions.collectAndFlushOnSilence
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.watch.{DirectoryState, DirectoryStateJvm, DirectoryWatch, DirectoryWatchSettings}
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.problem.Checked.catchNonFatal
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.Labeled
import js7.base.utils.ScalaUtils.checkedCast
import js7.base.utils.ScalaUtils.syntax.*
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

final class DirectoryWatchingSignatureVerifier private(
  companionToDirectory: Map[SignatureVerifier.Companion, Path],
  settings: DirectoryWatchSettings,
  onUpdated: () => Unit)
extends SignatureVerifier, Service.StoppableByCancel:

  @volatile private var state = State(Map.empty)

  protected type MySignature = GenericSignature

  def companion: DirectoryWatchingSignatureVerifier.type =
    DirectoryWatchingSignatureVerifier

  def publicKeys: Seq[String] =
    throw new NotImplementedError("DirectoryWatchingSignatureVerifier#publicKeys")

  def publicKeyOrigin: String =
    "(DirectoryWatchingSignatureVerifier)"

  protected def start =
    companionToDirectory
      .toVector
      .traverse: (companion, directory) =>
        readDirectory(directory)
          .map(_.orThrow) // Directory must be readable at start !!!
          .map: directoryState =>
            companion -> (directory -> directoryState)
      .flatMap: companionToDir =>
        IO.defer:
          state = State(
            companionToDir
              .map:
                case (companion, (directory, directoryState)) =>
                  companion -> toVerifier(companion, directory, directoryState)
              .toMap)

          startService:
            companionToDir
              .map { case (companion, (directory, directoryState)) =>
                CorrelId.bindNew:
                  watchDirectory(companion, directory, directoryState)
                    .onErrorRestartLoop(()): (t, _, retry) =>
                      IO.defer:
                        logger.error(s"${companion.typeName}: ${t.toStringWithCauses}",
                          t.nullIfNoStackTrace)
                        retry(()).delayBy(10.s)
              }
              .parSequence
              .map(_.combineAll)

  private def watchDirectory(
    companion: SignatureVerifier.Companion,
    directory: Path,
    directoryState: DirectoryState)
  : IO[Unit] =
    DirectoryWatch
      .stream(directory, directoryState, settings, isRelevantFile)
      .collectAndFlushOnSilence(settings.directorySilence)
      .evalTap(events => IO(logger.info:
        s"Rereading signature keys of ${companion.typeName} type due to ${
          events.asSeq.mkString(", ")}"))
      .foreach: _ =>
        rereadDirectory(directory)
      .compile
      .drain

  private def rereadDirectory(directory: Path): IO[Unit] =
    logger.debugIO(IO.defer:
      companionToDirectory
        .collect:
          case (companion, `directory`) =>
            readDirectory(directory)
              .flatMapT(directoryState => IO:
                toVerifier(companion, directory, directoryState))
              .map(companion -> _)
        .toVector
        .sequence
        .map: updated =>
          // Update state atomically:
          state = State(state.companionToVerifier ++ updated)
          try onUpdated()
          catch case NonFatal(t) =>
            logger.error(s"onUpdated => ${t.toStringWithCauses}", t.nullIfNoStackTrace))

  private def toVerifier(
    companion: SignatureVerifier.Companion,
    directory: Path,
    directoryState: DirectoryState)
  : Checked[SignatureVerifier] =
    val files = directoryState.files.toVector
    val checked = catchNonFatal(
      companion.ignoreInvalid(
        files.map(file => directory.resolve(file).labeledByteArray),
        origin = directory.toString))

    checked match
      case Left(problem) =>
        logger.error(s"${companion.typeName} signature keys are not readable: $problem")

      case Right(verifier) =>
        logger.info("Trusting signature keys:")
        for o <- verifier.publicKeysToStrings do logger.info(s"  $o")
        if files.isEmpty then
          logger.warn(
            s"  No public key files for signature verifier '${companion.typeName}' in directory '$directory'")

    checked

  private def readDirectory(directory: Path): IO[Checked[DirectoryState]] =
    DirectoryStateJvm.readDirectory(directory, isRelevantFile).catchAsChecked

  private def isRelevantFile(file: Path) =
    !file.getFileName.startsWith(".")

  override def verify(document: ByteArray, signature: GenericSignature): Checked[Seq[SignerId]] =
    state.genericVerifier.verify(document, signature)

  override def publicKeysToStrings: Seq[String] =
    "DirectoryWatchingSignatureVerifier#publicKeysToStrings" :: Nil

  override def toString =
    s"DirectoryWatchingSignatureVerifier(${companionToDirectory.keys.map(_.typeName).mkString(" ")})"


object DirectoryWatchingSignatureVerifier extends SignatureVerifier.Companion:
  private val configPath = "js7.configuration.trusted-signature-keys"
  private val logger = Logger[this.type]

  protected type MySignature = GenericSignature
  protected type MySignatureVerifier = DirectoryWatchingSignatureVerifier

  def typeName = "(generic)"

  def filenameExtension: String =
    throw new NotImplementedError

  def recommendedKeyDirectoryName: String =
    throw new NotImplementedError("DirectoryWatchingSignatureVerifier recommendedKeyDirectoryName")

  def checkedResource(config: Config, onUpdated: () => Unit)
  : Checked[ResourceIO[DirectoryWatchingSignatureVerifier]] =
    prepare(config).map(_.toResource(onUpdated))

  def prepare(config: Config): Checked[Prepared] =
    config.getObject(configPath).asScala.toMap  // All Config key-values
      .map: (typeName, v) =>
        checkedCast[String](v.unwrapped, ConfigStringExpectedProblem(s"$configPath.$typeName"))
          .map(Paths.get(_))
          .flatMap: directory =>
            SignatureServices.nameToSignatureVerifierCompanion
              .rightOr(typeName, UnknownSignatureTypeProblem(typeName))
              .flatMap: companion =>
                if !exists(directory) then
                  Left(Problem.pure:
                    s"Signature key directory '$directory' for '$typeName' does not exist")
                else
                  Right(companion -> directory)
      .toVector
      .sequence
      .map(_.toMap)
      .flatMap: companionToDirectory =>
        if companionToDirectory.isEmpty then
          Left(Problem.pure(s"No trusted signature keys - Configure one with $configPath!"))
        else
          for settings <- DirectoryWatchSettings.fromConfig(config) yield
            new Prepared(companionToDirectory, settings)

  final class Prepared(
    companionToDirectory: Map[SignatureVerifier.Companion, Path],
    settings: DirectoryWatchSettings):

    def toResource(onUpdated: () => Unit)
    : ResourceIO[DirectoryWatchingSignatureVerifier] =
      Service.resource:
        new DirectoryWatchingSignatureVerifier(companionToDirectory, settings, onUpdated)

  @deprecated("Not implemented", "")
  def checked(publicKeys: Seq[Labeled[ByteArray]], origin: String) =
    throw new NotImplementedError("DirectoryWatchingSignatureVerifier.checked?")

  @deprecated("Not implemented", "")
  def ignoreInvalid(publicKeys: Seq[Labeled[ByteArray]], origin: String) =
    throw new NotImplementedError("DirectoryWatchingSignatureVerifier.ignoreInvalid")

  def genericSignatureToSignature(signature: GenericSignature): Checked[GenericSignature] =
    Right(signature)

  private final case class State(
    companionToVerifier: Map[SignatureVerifier.Companion, Checked[SignatureVerifier]]):

    // Failing verifiers are omitted !!!
    val genericVerifier = new GenericSignatureVerifier(
      companionToVerifier.values
        .collect { case Right(o) => o }
        .toVector)

  private case class ConfigStringExpectedProblem(configKey: String) extends Problem.Lazy(
    s"String expected as value of configuration key $configKey")
