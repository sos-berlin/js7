package js7.base.crypt.generic

import cats.instances.either.*
import cats.instances.vector.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import java.nio.file.Files.exists
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY}
import java.nio.file.{Path, Paths}
import js7.base.Problems.UnknownSignatureTypeProblem
import js7.base.configutils.Configs.RichConfig
import js7.base.crypt.generic.DirectoryWatchingSignatureVerifier.{Settings, State, logger}
import js7.base.crypt.{GenericSignature, SignatureVerifier, SignerId}
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.watch.{DirectoryState, DirectoryWatcher, WatchOptions}
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked.{catchNonFatal, catchNonFatalFlatten}
import js7.base.problem.{Checked, Problem}
import js7.base.thread.Futures.implicits.*
import js7.base.thread.IOExecutor
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.ScalaUtils.checkedCast
import js7.base.utils.ScalaUtils.syntax.{RichThrowable, *}
import monix.eval.Task
import monix.execution.atomic.Atomic
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.blocking
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

final class DirectoryWatchingSignatureVerifier private(
  companionToDirectory: Map[SignatureVerifier.Companion, Path],
  settings: Settings,
  onUpdated: () => Unit)
  (implicit iox: IOExecutor)
extends SignatureVerifier with AutoCloseable
{
  @volatile private var state = State(
    companionToDirectory
      .map { case (companion, directory) =>
        val checkedVerifier = readDirectory(directory).flatMap(toVerifier(companion, directory, _))
        companion -> checkedVerifier
      })

  private var runningFuture = CancelableFuture.successful(())
  private val stop = PublishSubject[Unit]()
  private val stopped = Atomic(false)

  protected type MySignature = GenericSignature

  def companion = DirectoryWatchingSignatureVerifier

  def publicKeys = throw new NotImplementedError("DirectoryWatchingSignatureVerifier#key")

  def publicKeyOrigin = "(DirectoryWatchingSignatureVerifier)"

  def maybeProblem: Option[Problem] =
    Problem.combineAllOption(
      state.companionToVerifier.values.collect { case Left(problem) => problem })

  private def start()(implicit s: Scheduler): Unit = {
    val companionToDirectoryState =
      for ((companion, directory) <- companionToDirectory) yield
        companion -> (directory -> readDirectory(directory).orThrow)

    runningFuture = startAndForget(companionToDirectoryState)
      .onErrorHandle { t =>
        logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
      }
      .runToFuture
  }

  def close(): Unit =
    if (!stopped.getAndSet(true)) {
      stop.onComplete()
      blocking {
        // TODO Shutdown JS7 asynchronously !
        try runningFuture.await(5.s)
        catch { case NonFatal(t) =>
          logger.error(s"stop: ${t.toStringWithCauses}")
        }
      }
    }

  private def startAndForget(
    companionToDirectoryState: Map[SignatureVerifier.Companion, (Path, DirectoryState)])
    (implicit iox: IOExecutor)
  : Task[Unit] =
    companionToDirectoryState
      .toVector.distinct
      .map { case (companion, (directory, directoryState)) =>
        CorrelId.bindNew(
          observeDirectory(companion, directory, directoryState)
            .onErrorRestartLoop(()) {( t, _, retry) =>
              logger.error(s"${companion.typeName}: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
              retry(()).delayExecution(10.s)
            }
            .startAndForget)
      }
      .sequence
      .void

  private def observeDirectory(
    companion: SignatureVerifier.Companion,
    directory: Path,
    directoryState: DirectoryState)
    (implicit iox: IOExecutor)
  : Task[Unit] =
    DirectoryWatcher
      .observable(directoryState, toWatchOptions(directory))
      .takeUntil(stop)
      .flatMap(Observable.fromIterable)
      .debounce(settings.directorySilence)
      .bufferIntrospective(1024)
      .tapEach(events => logger.info(
        s"Rereading signature keys of ${companion.typeName} type due to ${
          events.distinct.mkString(", ")}"))
      .mapEval(_ =>
        rereadDirectory(directory))
      .completedL

  private def toWatchOptions(directory: Path): WatchOptions =
    WatchOptions(
      directory,
      Set(ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE),
      matches = isRelevantFile,
      retryDelays = settings.retryDelays,
      pollTimeout = settings.pollTimeout,
      delay = settings.watchDelay)

  private def rereadDirectory(directory: Path): Task[Unit] =
    logger.debugTask(Task.defer {
      companionToDirectory
        .collect { case (companion, `directory`) =>
          iox(Task(readDirectory(directory)))
            .flatMapT(directoryState => Task(
              toVerifier(companion, directory, directoryState)))
            .map(companion -> _)
        }
        .toVector
        .sequence
        .map { updated =>
          // Update state atomically:
          state = State(state.companionToVerifier ++ updated)

          try onUpdated()
          catch { case NonFatal(t) =>
            logger.error(s"onUpdated => ${t.toStringWithCauses}", t)
          }
        }
      })

  private def toVerifier(
    companion: SignatureVerifier.Companion,
    directory: Path,
    directoryState: DirectoryState)
  : Checked[SignatureVerifier] = {
    val files = directoryState.files.toVector
    val checked = catchNonFatalFlatten(
      companion.checked(
        files.map(file => directory.resolve(file).byteArray),
        origin = directory.toString))

    checked match {
      case Left(problem) =>
        logger.error(s"${companion.typeName} signature keys are not readable: $problem")

      case Right(verifier) =>
        logger.info("Trusting signature key")
        for (o <- verifier.publicKeysToStrings) logger.info(s"  $o")
        if (files.isEmpty) {
          logger.warn(
            s"  No public key files for signature verifier '${companion.typeName}' in directory '$directory'")
        }
    }

    checked
  }

  private def readDirectory(directory: Path): Checked[DirectoryState] =
    catchNonFatal(
      DirectoryState.readDirectory(directory, isRelevantFile))

  private def isRelevantFile(file: Path) =
    !file.getFileName.startsWith(".")

  override def verify(document: ByteArray, signature: GenericSignature): Checked[Seq[SignerId]] =
    state.genericVerifier.verify(document, signature)

  override def publicKeysToStrings =
    "DirectoryWatchingSignatureVerifier#publicKeysToStrings" :: Nil
}

object DirectoryWatchingSignatureVerifier extends SignatureVerifier.Companion
{
  private val configPath = "js7.configuration.trusted-signature-keys"
  private val logger = Logger(getClass)

  protected type MySignature = GenericSignature
  protected type MySignatureVerifier = DirectoryWatchingSignatureVerifier

  def typeName = "(generic)"

  def filenameExtension =
    throw new NotImplementedError

  def recommendedKeyDirectoryName =
    throw new NotImplementedError("DirectoryWatchingSignatureVerifier recommendedKeyDirectoryName")

  def start(
    config: Config,
    onUpdated: () => Unit = () => ())(
    implicit s: Scheduler, iox: IOExecutor)
  : Checked[DirectoryWatchingSignatureVerifier] =
    checked(config, onUpdated)
      .tapEach(_.start())

  private def checked(config: Config, onUpdated: () => Unit)(implicit iox: IOExecutor)
  : Checked[DirectoryWatchingSignatureVerifier] =
    config.getObject(configPath).asScala.toMap  // All Config key-values
      .map { case (typeName, v) =>
        checkedCast[String](v.unwrapped, ConfigStringExpectedProblem(s"$configPath.$typeName"))
          .map(Paths.get(_))
          .flatMap(directory => SignatureServices
            .nameToSignatureVerifierCompanion
            .rightOr(typeName, UnknownSignatureTypeProblem(typeName))
            .flatMap(companion =>
              if (!exists(directory))
                Left(Problem.pure(
                  s"Signature key directory '$directory' for '$typeName' does not exists"))
              else
                Right(companion -> directory)))
      }
      .toVector
      .sequence
      .map(_.toMap)
      .flatMap { companionToDirectory =>
        if (companionToDirectory.isEmpty)
          Left(Problem.pure(s"No trusted signature keys - Configure one with $configPath!"))
        else
          for (settings <- Settings.fromConfig(config)) yield
            new DirectoryWatchingSignatureVerifier(companionToDirectory, settings, onUpdated)
      }

  @deprecated("Not implemented", "")
  def checked(publicKeys: Seq[ByteArray], origin: String) =
    throw new NotImplementedError("DirectoryWatchingSignatureVerifier.checked?")

  def genericSignatureToSignature(signature: GenericSignature) =
    Right(signature)

  private final case class State(
    companionToVerifier: Map[SignatureVerifier.Companion, Checked[SignatureVerifier]])
  {
    // Failing verifiers are omitted !!!
    val genericVerifier = new GenericSignatureVerifier(
      companionToVerifier.values
        .collect { case Right(o) => o }
        .toVector)
  }

  final case class Settings(
    watchDelay: FiniteDuration,
    pollTimeout: FiniteDuration,
    retryDelays: Seq[FiniteDuration],
    directorySilence: FiniteDuration,
    logDelays: Seq[FiniteDuration])
  object Settings {
    def fromConfig(config: Config): Checked[Settings] =
      for {
        watchDelay <- config.finiteDuration(
          "js7.configuration.trusted-signature-key-settings.watch-delay")
        pollTimeout <- config.finiteDuration(
          "js7.configuration.trusted-signature-key-settings.poll-timeout")
        retryDelays <- catchNonFatal(config.getDurationList(
          "js7.configuration.trusted-signature-key-settings.retry-delays")
          .asScala.map(_.toFiniteDuration).toVector)
        directorySilence <- config.finiteDuration(
          "js7.configuration.trusted-signature-key-settings.directory-silence")
        logDelays <- catchNonFatal(config.getDurationList(
          "js7.configuration.trusted-signature-key-settings.log-delays")
          .asScala.map(_.toFiniteDuration).toVector)
      } yield
        Settings(watchDelay, pollTimeout, retryDelays, directorySilence, logDelays)
  }

  private case class ConfigStringExpectedProblem(configKey: String) extends Problem.Lazy(
    s"String expected as value of configuration key $configKey")
}
