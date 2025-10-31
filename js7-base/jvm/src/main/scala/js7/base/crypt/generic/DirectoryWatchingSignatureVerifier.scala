package js7.base.crypt.generic

import cats.effect
import cats.effect.{IO, ResourceIO}
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import java.nio.file.Path
import js7.base.catsutils.CatsEffectExtensions.catchAsChecked
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier.*
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
import js7.base.time.WallClock
import js7.base.utils.Labeled
import js7.base.utils.ScalaUtils.syntax.*
import scala.util.control.NonFatal

final class DirectoryWatchingSignatureVerifier private(
  companionToDirectory: Map[SignatureVerifier.Provider, Path],
  settings: DirectoryWatchSettings,
  onUpdated: () => Unit,
  val provider: DirectoryWatchingSignatureVerifier.Provider)
extends
  SignatureVerifier, Service.StoppableByCancel:

  import provider.State

  @volatile private var state = State(Map.empty)

  protected type MySignature = GenericSignature

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
          state = provider.State:
            companionToDir.map:
              case (companion, (directory, directoryState)) =>
                companion -> toVerifier(companion, directory, directoryState)
            .toMap

          startService:
            companionToDir.map:
              case (companion, (directory, directoryState)) =>
                CorrelId.bindNew:
                  watchDirectory(companion, directory, directoryState)
                    .onErrorRestartLoop(()): (t, _, retry) =>
                      IO.defer:
                        logger.error(s"${companion.typeName}: ${t.toStringWithCauses}",
                          t.nullIfNoStackTrace)
                        retry(()).delayBy(10.s)
            .parSequence
            .map(_.combineAll)

  private def watchDirectory(
    provider: SignatureVerifier.Provider,
    directory: Path,
    directoryState: DirectoryState)
  : IO[Unit] =
    DirectoryWatch
      .stream(directory, directoryState, settings, isRelevantFile)
      .collectAndFlushOnSilence(settings.directorySilence)
      .evalTap(events => IO(logger.info:
        s"Rereading signature keys of ${provider.typeName} type due to ${
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
    provider: SignatureVerifier.Provider,
    directory: Path,
    directoryState: DirectoryState)
  : Checked[SignatureVerifier] =
    val files = directoryState.files.toVector
    val checked = catchNonFatal(
      provider.ignoreInvalid(
        files.map(file => directory.resolve(file).labeledByteArray),
        origin = directory.toString))

    checked match
      case Left(problem) =>
        logger.error(s"${provider.typeName} signature keys are not readable: $problem")

      case Right(verifier) =>
        logger.info("Trusting signature keys:")
        for o <- verifier.publicKeysToStrings do logger.info(s"  $o")
        if files.isEmpty then
          logger.warn(
            s"  No public key files for signature verifier '${provider.typeName}' in directory '$directory'")

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


object DirectoryWatchingSignatureVerifier:
  private val logger = Logger[this.type]
  private val configPath = "js7.configuration.trusted-signature-keys"

  private case class ConfigStringExpectedProblem(configKey: String) extends Problem.Lazy(
    s"String expected as value of configuration key $configKey")


  final class Provider(clock: WallClock, config: Config)
  extends SignatureVerifier.Provider:

    protected type MySignature = GenericSignature
    protected type MySignatureVerifier = DirectoryWatchingSignatureVerifier

    private val genericSignatureVerifierProvider = GenericSignatureVerifier.Provider(clock, config)

    def typeName = "(generic)"

    def filenameExtension: String =
      throw new NotImplementedError

    def recommendedKeyDirectoryName: String =
      throw new NotImplementedError("DirectoryWatchingSignatureVerifier recommendedKeyDirectoryName")

    def checkedResource(config: Config, onUpdated: () => Unit)
    : Checked[ResourceIO[DirectoryWatchingSignatureVerifier]] =
      prepare.map(_.toResource(onUpdated))

    def prepare: Checked[Prepared] =
      genericSignatureVerifierProvider.readNonEmptyProviderToDirectory
        .flatMap: providerToDirectory =>
          DirectoryWatchSettings.fromConfig(config).map: settings =>
            new Prepared(providerToDirectory.toVector.toMap, settings)

    final class Prepared(
      companionToDirectory: Map[SignatureVerifier.Provider, Path],
      settings: DirectoryWatchSettings):

      def toResource(onUpdated: () => Unit)
      : ResourceIO[DirectoryWatchingSignatureVerifier] =
        Service.resource:
          DirectoryWatchingSignatureVerifier(companionToDirectory, settings, onUpdated,
            Provider.this)

    @deprecated("Not implemented", "")
    def checked(publicKeys: Seq[Labeled[ByteArray]], origin: String) =
      throw new NotImplementedError("DirectoryWatchingSignatureVerifier.checked?")

    @deprecated("Not implemented", "")
    def ignoreInvalid(publicKeys: Seq[Labeled[ByteArray]], origin: String) =
      throw new NotImplementedError("DirectoryWatchingSignatureVerifier.ignoreInvalid")

    def genericSignatureToSignature(signature: GenericSignature): Checked[GenericSignature] =
      Right(signature)


    private[DirectoryWatchingSignatureVerifier] final case class State(
      companionToVerifier: Map[SignatureVerifier.Provider, Checked[SignatureVerifier]]):

      // Failing verifiers are omitted !!!
      val genericVerifier = GenericSignatureVerifier(
        companionToVerifier.values
          .collect:
            case Right(o) => o
          .toVector,
        genericSignatureVerifierProvider)
