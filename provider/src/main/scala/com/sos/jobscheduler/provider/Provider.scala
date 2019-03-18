package com.sos.jobscheduler.provider

import cats.data.Validated.{Invalid, Valid}
import cats.instances.vector._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.convert.As._
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.files.{DirectoryReader, PathSeqDiff, PathSeqDiffer}
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.{HasCloser, IOExecutor, Logger}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.crypt.generic.MessageSigners
import com.sos.jobscheduler.core.filebased.FileBaseds.diffFileBaseds
import com.sos.jobscheduler.core.filebased.{FileBasedSigner, FileBaseds, TypedPaths, TypedSourceReader}
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.filebased.{FileBased, TypedPath, VersionId}
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
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.concurrent.duration._

// Test in com.sos.jobscheduler.tests.provider.ProviderTest
/**
  * @author Joacim Zschimmer
  */
final class Provider(val fileBasedSigner: FileBasedSigner[FileBased], val conf: ProviderConfiguration)(implicit val s: Scheduler)
extends HasCloser with Observing
{
  protected val masterApi = new AkkaHttpMasterApi(conf.masterUri, config = conf.config)

  private val userAndPassword: Option[UserAndPassword] = for {
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

  protected val relogin: Task[Unit] =
    masterApi.logout().onErrorHandle(_ => ()) >>
      masterApi.login(userAndPassword)

  // We don't use ReplaceRepo because it changes every existing object only because of changed signature.
  private def replaceMasterConfiguration(versionId: Option[VersionId] = None): Task[Checked[Completed]] =
    for {
      _ <- loginUntilReachable
      currentEntries = readDirectory
      checkedCommand = toReplaceRepoCommand(versionId getOrElse newVersionId(), currentEntries.map(_.file))
      response <- checkedCommand
        .traverse(o => AkkaHttpClient.liftProblem(
          masterApi.executeCommand(o) map ((_: MasterCommand.Response) => Completed)))
        .map(_.flatten)
    } yield {
      if (response.isValid) {
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

  def testMasterDiff = masterDiff(readDirectory)

  /** Compares the directory with the Master's repo and sends the difference.
    * Parses each file, so it may take some time for a big configuration directory. */
  private def masterDiff(localEntries: Seq[DirectoryReader.Entry]): Task[Checked[FileBaseds.Diff[TypedPath, FileBased]]] =
    for {
      _ <- loginUntilReachable
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
          Invalid(problem)
        } else
          Valid(completed))

  private lazy val loginUntilReachable: Task[Unit] =
    Task {
      if (masterApi.hasSession)
        Task.unit
      else
        masterApi.loginUntilReachable(userAndPassword, retryLoginDurations, logThrowable)
          .map { _ =>
            logger.info("Logged in at Master")
            ()
          }
    }.flatten

  private def execute(versionId: Option[VersionId], diff: FileBaseds.Diff[TypedPath, FileBased]): Task[Checked[Completed.type]] =
    if (diff.isEmpty && versionId.isEmpty)
      Task(Checked.completed)
    else {
      val v = versionId getOrElse newVersionId()
      logUpdate(v, diff)
      AkkaHttpClient.liftProblem(
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
    for {
      stampedAgents <- masterApi.agents
      stampedWorkflows <- masterApi.workflows
    } yield stampedAgents.value ++ stampedWorkflows.value

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
    Valid(new Provider(fileBasedSigner, conf))
  }

  def observe(conf: ProviderConfiguration)(implicit s: Scheduler, iox: IOExecutor): Checked[Observable[Completed]] =
    for (provider <- Provider(conf)) yield
      provider.observe
        .guarantee(provider.closeTask.map((_: Completed) => ()))

  private def logThrowable(throwable: Throwable): Unit = {
    logger.error(throwable.toStringWithCauses)
    logger.debug(throwable.toString, throwable)
  }
}
