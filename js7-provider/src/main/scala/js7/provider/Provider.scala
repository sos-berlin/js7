package js7.provider

import cats.implicits._
import com.typesafe.config.ConfigUtil
import java.nio.file.{Files, Path, Paths}
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.convert.As._
import js7.base.generic.{Completed, SecretString}
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.HasCloser
import js7.base.web.HttpClient
import js7.common.akkautils.ProvideActorSystem
import js7.common.configutils.Configs.ConvertibleConfig
import js7.common.files.{DirectoryReader, PathSeqDiff, PathSeqDiffer}
import js7.common.scalautil.{IOExecutor, Logger}
import js7.common.time.JavaTimeConverters._
import js7.controller.agent.AgentRefReader
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.data.ControllerCommand
import js7.controller.data.ControllerCommand.{ReplaceRepo, UpdateRepo}
import js7.controller.workflow.WorkflowReader
import js7.core.crypt.generic.MessageSigners
import js7.core.filebased.{TypedPaths, TypedSourceReader}
import js7.data.agent.AgentRefPath
import js7.data.controller.ControllerFileBaseds
import js7.data.filebased.FileBaseds.diffFileBaseds
import js7.data.filebased.{FileBased, FileBasedSigner, FileBaseds, TypedPath, VersionId}
import js7.data.workflow.WorkflowPath
import js7.provider.Provider._
import js7.provider.configuration.ProviderConfiguration
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicAny
import monix.reactive.Observable
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

// Test in js7.tests.provider.ProviderTest
/**
  * @author Joacim Zschimmer
  */
final class Provider(val fileBasedSigner: FileBasedSigner[FileBased], val conf: ProviderConfiguration)(implicit val s: Scheduler)
extends HasCloser with Observing with ProvideActorSystem
{
  protected val userAndPassword: Option[UserAndPassword] = for {
      userName <- conf.config.optionAs[String]("js7.provider.controller.user")
      password <- conf.config.optionAs[String]("js7.provider.controller.password")
    } yield UserAndPassword(UserId(userName), SecretString(password))
  protected val controllerApi = new AkkaHttpControllerApi(conf.controllerUri, userAndPassword, actorSystem = actorSystem, config = conf.config)
  protected def config = conf.config

  private val firstRetryLoginDurations = conf.config.getDurationList("js7.provider.controller.login-retry-delays")
    .asScala.map(_.toFiniteDuration)
  private val typedSourceReader = new TypedSourceReader(conf.liveDirectory, readers)
  private val newVersionId = new VersionIdGenerator
  private val lastEntries = AtomicAny(Vector.empty[DirectoryReader.Entry])

  protected def scheduler = s

  def closeTask: Task[Completed] =
    controllerApi.logout()
      .guarantee(Task(controllerApi.close()))
      .memoize

  protected val relogin: Task[Completed] =
    controllerApi.logout().onErrorHandle(_ => ()) >>
      loginUntilReachable

  // We don't use ReplaceRepo because it changes every existing object only because of changed signature.
  private def replaceControllerConfiguration(versionId: Option[VersionId]): Task[Checked[Completed]] =
    for {
      _ <- loginUntilReachable
      currentEntries = readDirectory
      checkedCommand = toReplaceRepoCommand(versionId getOrElse newVersionId(), currentEntries.map(_.file))
      response <- checkedCommand
        .traverse(o => HttpClient.liftProblem(
          controllerApi.executeCommand(o) map ((_: ControllerCommand.Response) => Completed)))
        .map(_.flatten)
    } yield {
      if (response.isRight) {
        lastEntries := currentEntries
      }
      response
    }

  /** Compares the directory with the Controller's repo and sends the difference.
    * Parses each file, so it may take some time for a big configuration directory. */
  def initiallyUpdateControllerConfiguration(versionId: Option[VersionId] = None): Task[Checked[Completed]] = {
    val localEntries = readDirectory
    for {
      _ <- loginUntilReachable
      checkedDiff <- controllerDiff(localEntries)
      checkedCompleted <- checkedDiff
        .traverse(execute(versionId, _))
        .map(_.flatten)
    } yield {
      for (_ <- checkedCompleted) {
        lastEntries := localEntries
      }
      checkedCompleted
    }
  }

  def testControllerDiff: Task[Checked[FileBaseds.Diff[TypedPath, FileBased]]] =
    loginUntilReachable >> controllerDiff(readDirectory)

  /** Compares the directory with the Controller's repo and sends the difference.
    * Parses each file, so it may take some time for a big configuration directory. */
  private def controllerDiff(localEntries: Seq[DirectoryReader.Entry]): Task[Checked[FileBaseds.Diff[TypedPath, FileBased]]] =
    for {
      pair <- Task.parZip2(readLocalFileBased(localEntries.map(_.file)), fetchControllerFileBasedSeq)
      (checkedLocalFileBasedSeq, controllerFileBasedSeq) = pair
    } yield
      checkedLocalFileBasedSeq.map(o => fileBasedDiff(o, controllerFileBasedSeq))

  private def readLocalFileBased(files: Seq[Path]) =
    Task { typedSourceReader.readFileBaseds(files) }

  private def fileBasedDiff(aSeq: Seq[FileBased], bSeq: Seq[FileBased]): FileBaseds.Diff[TypedPath, FileBased] =
    FileBaseds.Diff.fromRepoChanges(diffFileBaseds(aSeq, bSeq, ignoreVersion = true))

  def updateControllerConfiguration(versionId: Option[VersionId] = None): Task[Checked[Completed]] =
    for {
      _ <- loginUntilReachable
      last = lastEntries.get
      currentEntries = readDirectory
      checkedCompleted <- toFileBasedDiff(PathSeqDiffer.diff(currentEntries, last))
        .traverse(
          execute(versionId, _))
        .map(_.flatten)
    } yield
      checkedCompleted flatMap (completed =>
        if (!lastEntries.compareAndSet(last, currentEntries)) {
          val problem = Problem.pure("Provider has been concurrently used")
          logger.debug(problem.toString)
          Left(problem)
        } else
          Right(completed))

  protected lazy val loginUntilReachable: Task[Completed] =
    Task.defer {
      if (controllerApi.hasSession)
        Task.pure(Completed)
      else
        controllerApi.loginUntilReachable(retryLoginDurations)
          .map { completed =>
            logger.info("Logged-in at Controller")
            completed
          }
    }

  private def execute(versionId: Option[VersionId], diff: FileBaseds.Diff[TypedPath, FileBased]): Task[Checked[Completed.type]] =
    if (diff.isEmpty && versionId.isEmpty)
      Task(Checked.completed)
    else {
      val v = versionId getOrElse newVersionId()
      logUpdate(v, diff)
      HttpClient.liftProblem(
        controllerApi.executeCommand(toUpdateRepo(v, diff)) map ((_: ControllerCommand.Response) => Completed))
    }

  private def logUpdate(versionId: VersionId, diff: FileBaseds.Diff[TypedPath, FileBased]): Unit = {
    logger.info(s"Version ${versionId.string}")
    for (o <- diff.deleted            .sorted) logger.info(s"Delete ${o.pretty}")
    for (o <- diff.added  .map(_.path).sorted) logger.info(s"Add ${o.pretty}")
    for (o <- diff.updated.map(_.path).sorted) logger.info(s"Update ${o.pretty}")
  }

  private def toUpdateRepo(versionId: VersionId, diff: FileBaseds.Diff[TypedPath, FileBased]) =
    UpdateRepo(
      versionId,
      change = diff.added ++ diff.updated map (_ withVersion versionId) map fileBasedSigner.sign,
      delete = diff.deleted)

  private def fetchControllerFileBasedSeq: Task[Seq[FileBased]] =
    Task.parMap2(
      controllerApi.agents.map(_.orThrow),
      controllerApi.workflows.map(_.orThrow)
    )(_ ++ _)

  private def readDirectory: Vector[DirectoryReader.Entry] =
    DirectoryReader.entries(conf.liveDirectory).toVector

  private def toFileBasedDiff(diff: PathSeqDiff): Checked[FileBaseds.Diff[TypedPath, FileBased]] = {
    val checkedAdded = typedSourceReader.readFileBaseds(diff.added)
    val checkedChanged = typedSourceReader.readFileBaseds(diff.changed)
    val checkedDeleted: Checked[Vector[TypedPath]] =
      diff.deleted.toVector
        .traverse(path => TypedPaths.fileToTypedPath(typedPathCompanions, conf.liveDirectory, path))
    (checkedAdded, checkedChanged, checkedDeleted) mapN ((add, chg, del) => FileBaseds.Diff(add, chg, del))
  }

  private def toReplaceRepoCommand(versionId: VersionId, files: Seq[Path]): Checked[ReplaceRepo] =
    typedSourceReader.readFileBaseds(files)
      .map(fileBaseds => ReplaceRepo(versionId, fileBaseds map (x => fileBasedSigner.sign(x withVersion versionId))))

  private def retryLoginDurations: Iterator[FiniteDuration] =
    firstRetryLoginDurations.iterator ++ Iterator.continually(firstRetryLoginDurations.lastOption getOrElse 10.seconds)
}

object Provider
{
  private val typedPathCompanions = Set(AgentRefPath, WorkflowPath)
  private val logger = Logger(getClass)
  private val readers = AgentRefReader :: WorkflowReader :: Nil

  def apply(conf: ProviderConfiguration)(implicit s: Scheduler): Checked[Provider] = {
    val fileBasedSigner = {
      val typeName = conf.config.getString("js7.provider.sign-with")
      val configPath = "js7.provider.private-signature-keys." + ConfigUtil.quoteString(typeName)
      val keyFile = Paths.get(conf.config.getString(s"$configPath.key"))
      val password = SecretString(conf.config.getString(s"$configPath.password"))
      MessageSigners.typeToMessageSignersCompanion(typeName)
        .flatMap(companion => companion.checked(Files.readAllBytes(keyFile), password))
        .map(messageSigner => new FileBasedSigner(messageSigner, ControllerFileBaseds.jsonCodec))
    }.orThrow
    Right(new Provider(fileBasedSigner, conf))
  }

  def observe(conf: ProviderConfiguration)(implicit s: Scheduler, iox: IOExecutor): Checked[Observable[Completed]] =
    for (provider <- Provider(conf)) yield
      provider.observe
        .guarantee(provider.closeTask.map((_: Completed) => ()))
}
