package com.sos.jobscheduler.provider

import cats.implicits._
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.convert.As._
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.HasCloser
import com.sos.jobscheduler.base.web.HttpClient
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.files.{DirectoryReader, PathSeqDiff, PathSeqDiffer}
import com.sos.jobscheduler.common.scalautil.{IOExecutor, Logger}
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.sos.jobscheduler.core.crypt.generic.MessageSigners
import com.sos.jobscheduler.core.filebased.FileBaseds.diffFileBaseds
import com.sos.jobscheduler.core.filebased.{FileBaseds, TypedPaths, TypedSourceReader}
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedSigner, TypedPath, VersionId}
import com.sos.jobscheduler.data.master.MasterFileBaseds
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.agent.AgentRefReader
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.MasterCommand.{ReplaceRepo, UpdateRepo}
import com.sos.jobscheduler.master.workflow.WorkflowReader
import com.sos.jobscheduler.provider.Provider._
import com.sos.jobscheduler.provider.configuration.ProviderConfiguration
import com.typesafe.config.ConfigUtil
import java.nio.file.{Files, Path, Paths}
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicAny
import monix.reactive.Observable
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

// Test in com.sos.jobscheduler.tests.provider.ProviderTest
/**
  * @author Joacim Zschimmer
  */
final class Provider(val fileBasedSigner: FileBasedSigner[FileBased], val conf: ProviderConfiguration)(implicit val s: Scheduler)
extends HasCloser with Observing with ProvideActorSystem
{
  protected val masterApi = AkkaHttpMasterApi(conf.masterUri, actorSystem, conf.config)
  protected def config = conf.config

  protected val userAndPassword: Option[UserAndPassword] = for {
      userName <- conf.config.optionAs[String]("jobscheduler.provider.master.user")
      password <- conf.config.optionAs[String]("jobscheduler.provider.master.password")
    } yield UserAndPassword(UserId(userName), SecretString(password))
  private val firstRetryLoginDurations = conf.config.getDurationList("jobscheduler.provider.master.login-retry-delays")
    .asScala.map(_.toFiniteDuration)
  private val typedSourceReader = new TypedSourceReader(conf.liveDirectory, readers)
  private val newVersionId = new VersionIdGenerator
  private val lastEntries = AtomicAny(Vector.empty[DirectoryReader.Entry])

  protected def scheduler = s

  def closeTask: Task[Completed] =
    masterApi.logout()
      .guarantee(Task(masterApi.close()))
      .memoize

  protected val relogin: Task[Completed] =
    masterApi.logout().onErrorHandle(_ => ()) >>
      loginUntilReachable

  // We don't use ReplaceRepo because it changes every existing object only because of changed signature.
  private def replaceMasterConfiguration(versionId: Option[VersionId]): Task[Checked[Completed]] =
    for {
      _ <- loginUntilReachable
      currentEntries = readDirectory
      checkedCommand = toReplaceRepoCommand(versionId getOrElse newVersionId(), currentEntries.map(_.file))
      response <- checkedCommand
        .traverse(o => HttpClient.liftProblem(
          masterApi.executeCommand(o) map ((_: MasterCommand.Response) => Completed)))
        .map(_.flatten)
    } yield {
      if (response.isRight) {
        lastEntries := currentEntries
      }
      response
    }

  /** Compares the directory with the Master's repo and sends the difference.
    * Parses each file, so it may take some time for a big configuration directory. */
  def initiallyUpdateMasterConfiguration(versionId: Option[VersionId] = None): Task[Checked[Completed]] = {
    val localEntries = readDirectory
    for {
      _ <- loginUntilReachable
      checkedDiff <- masterDiff(localEntries)
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

  def testMasterDiff: Task[Checked[FileBaseds.Diff[TypedPath, FileBased]]] =
    loginUntilReachable >> masterDiff(readDirectory)

  /** Compares the directory with the Master's repo and sends the difference.
    * Parses each file, so it may take some time for a big configuration directory. */
  private def masterDiff(localEntries: Seq[DirectoryReader.Entry]): Task[Checked[FileBaseds.Diff[TypedPath, FileBased]]] =
    for {
      pair <- Task.parZip2(readLocalFileBased(localEntries.map(_.file)), fetchMasterFileBasedSeq)
      (checkedLocalFileBasedSeq, masterFileBasedSeq) = pair
    } yield
      checkedLocalFileBasedSeq.map(o => fileBasedDiff(o, masterFileBasedSeq))

  private def readLocalFileBased(files: Seq[Path]) =
    Task { typedSourceReader.readFileBaseds(files) }

  private def fileBasedDiff(aSeq: Seq[FileBased], bSeq: Seq[FileBased]): FileBaseds.Diff[TypedPath, FileBased] =
    FileBaseds.Diff.fromRepoChanges(diffFileBaseds(aSeq, bSeq, ignoreVersion = true))

  def updateMasterConfiguration(versionId: Option[VersionId] = None): Task[Checked[Completed]] =
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
      if (masterApi.hasSession)
        Task.pure(Completed)
      else
        masterApi.loginUntilReachable(userAndPassword, retryLoginDurations)
          .map { completed =>
            logger.info("Logged-in at Master")
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
        masterApi.executeCommand(toUpdateRepo(v, diff)) map ((_: MasterCommand.Response) => Completed))
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

  private def fetchMasterFileBasedSeq: Task[Seq[FileBased]] =
    Task.parMap2(
      masterApi.agents.map(_.orThrow),
      masterApi.workflows.map(_.orThrow)
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
      val typeName = conf.config.getString("jobscheduler.provider.sign-with")
      val configPath = "jobscheduler.provider.private-signature-keys." + ConfigUtil.quoteString(typeName)
      val keyFile = Paths.get(conf.config.getString(s"$configPath.key"))
      val password = SecretString(conf.config.getString(s"$configPath.password"))
      MessageSigners.typeToMessageSignersCompanion(typeName)
        .flatMap(companion => companion.checked(Files.readAllBytes(keyFile), password))
        .map(messageSigner => new FileBasedSigner(messageSigner, MasterFileBaseds.jsonCodec))
    }.orThrow
    Right(new Provider(fileBasedSigner, conf))
  }

  def observe(conf: ProviderConfiguration)(implicit s: Scheduler, iox: IOExecutor): Checked[Observable[Completed]] =
    for (provider <- Provider(conf)) yield
      provider.observe
        .guarantee(provider.closeTask.map((_: Completed) => ()))
}
