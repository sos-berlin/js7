package com.sos.jobscheduler.provider

import cats.data.Validated.Valid
import cats.instances.vector._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.convert.As._
import com.sos.jobscheduler.base.generic.{Completed, SecretString}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.files.{DirectoryReader, PathSeqDiff, PathSeqDiffer}
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.MonixUtils.autoCloseableToObservable
import com.sos.jobscheduler.common.scalautil.{IOExecutor, Logger}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.crypt.generic.MessageSigners
import com.sos.jobscheduler.core.filebased.FileBasedReader.readObjects
import com.sos.jobscheduler.core.filebased.FileBaseds.diffFileBaseds
import com.sos.jobscheduler.core.filebased.{FileBasedSigner, FileBaseds, TypedPathDirectoryWalker}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.{FileBased, TypedPath, VersionId}
import com.sos.jobscheduler.data.master.MasterFileBaseds
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.agent.AgentReader
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
import scala.compat.Platform.ConcurrentModificationException
import scala.concurrent.duration._

// Test in com.sos.jobscheduler.tests.provider.ProviderTest
/**
  * @author Joacim Zschimmer
  */
final class Provider(val fileBasedSigner: FileBasedSigner, val conf: ProviderConfiguration)(implicit scheduler: Scheduler)
extends AutoCloseable with Observing
{
  protected val api = new AkkaHttpMasterApi(conf.masterUri, config = conf.config)
  private val userAndPassword: Option[UserAndPassword] = for {
      userName ← conf.config.optionAs[String]("jobscheduler.provider.master.user")
      password ← conf.config.optionAs[String]("jobscheduler.provider.master.password")
    } yield UserAndPassword(UserId(userName), SecretString(password))
  private val firstRetryLoginDurations = conf.config.getDurationList("jobscheduler.provider.master.login-retry-delays")
    .asScala.map(_.toFiniteDuration)
  private val newVersionId = new VersionIdGenerator

  private val lastEntries = AtomicAny(Vector.empty[DirectoryReader.Entry])

  def close() = closeTask.runAsyncAndForget

  def closeTask: Task[Completed] =
    api.logout()
      .guarantee(Task(api.close()))

  // TODO MasterOrderKeeper does not support change of Agents yet
  private def replaceMasterConfiguration(versionId: Option[VersionId] = None): Task[Checked[Completed]] =
    for {
      _ ← loginUntilReachable
      currentEntries = readDirectory
      checkedCommand = toReplaceRepoCommand(versionId getOrElse newVersionId(), currentEntries map (_.path))
      response ← checkedCommand
        .traverse(o ⇒ AkkaHttpClient.liftProblem(
          api.executeCommand(o) map ((_: MasterCommand.Response) => Completed)))
        .map(_.flatten)
    } yield {
      if (response.isValid) {
        lastEntries := currentEntries
      }
      response
    }

  /** Compares the directory with the Master's repo and sends the difference.
    * Parses each file, so it may take some time for a big configuration directory. */
  def initialUpdateMasterConfiguration(versionId: Option[VersionId] = None): Task[Checked[Completed]] =
    for {
      _ ← loginUntilReachable
      currentEntries = readDirectory
      master <- masterFileBased
      checked <- readFileBased(currentEntries map (_.path))
        .map { fileBased =>
          val v = versionId getOrElse newVersionId()
          val diff = FileBaseds.Diff.fromRepoChanges(
            diffFileBaseds(fileBased map (_ withVersion v), master, ignoreVersion = true))
          MasterCommand.UpdateRepo(
            v,
            (diff.added ++ diff.updated) map fileBasedSigner.sign,
            diff.deleted)
        }
        .traverse(execute(versionId, _))
        .map(_.flatten)
    } yield {
      if (checked.isValid) {
        lastEntries := currentEntries
      }
      checked
    }

  def updateMasterConfiguration(versionId: Option[VersionId] = None): Task[Checked[Completed]] =
    for {
      _ ← loginUntilReachable
      last = lastEntries.get
      currentEntries = readDirectory
      response ← toUpdateRepoCommand(versionId getOrElse newVersionId(), PathSeqDiffer.diff(currentEntries, last))
        .traverse(execute(versionId, _))
        .map(_.flatten)
    } yield {
      if (response.isValid) {
        if (!lastEntries.compareAndSet(last, currentEntries))
          throw new ConcurrentModificationException("Provider has been concurrently used")
      }
      response
    }

  private lazy val loginUntilReachable: Task[Unit] =
    Task {
      if (api.hasSession)
        Task.unit
      else
        api.loginUntilReachable(userAndPassword, retryLoginDurations, logThrowable).map(_ ⇒ ())
    }.flatten


  private def execute(versionId: Option[VersionId], updateRepo: UpdateRepo): Task[Checked[Completed.type]] =
    if (updateRepo.isEmpty && versionId.isEmpty)
      Task(Checked.completed)
    else
      AkkaHttpClient.liftProblem(
        api.executeCommand(updateRepo) map ((_: MasterCommand.Response) ⇒ Completed))

  private def masterFileBased: Task[Seq[FileBased]] =
    for {
      stampedAgents <- api.agents
      stampedWorkflows <- api.workflows
    } yield stampedAgents.value ++ stampedWorkflows.value

  private def readDirectory = DirectoryReader.entries(conf.liveDirectory)

  private def toUpdateRepoCommand(versionId: VersionId, diff: PathSeqDiff): Checked[UpdateRepo] = {
    val checkedChanged = readFileBased(diff.added ++ diff.changed)
    val checkedDeleted: Checked[Vector[TypedPath]] =
      diff.deleted.map(path ⇒ TypedPathDirectoryWalker.fileToTypedFile(conf.liveDirectory, path, typedPathCompanions))
        .toVector.sequence
        .map(_ map (_.path))
    (checkedChanged, checkedDeleted) mapN ((chg, del) ⇒ UpdateRepo(versionId, chg map (o ⇒ fileBasedSigner.sign(o withVersion versionId)), del))
  }

  private def readFileBased(paths: Seq[Path]): Checked[Vector[FileBased]] =
    paths.map(path ⇒ TypedPathDirectoryWalker.fileToTypedFile(conf.liveDirectory, path, typedPathCompanions))
    .toVector.sequence
    .flatMap(readObjects(readers, conf.liveDirectory, _))
    .map(_.toVector)

  private def toReplaceRepoCommand(versionId: VersionId, files: Seq[Path]): Checked[ReplaceRepo] = {
    val checkedFileBased: Checked[Vector[FileBased]] =
      files.toVector
        .traverse(file ⇒ TypedPathDirectoryWalker.fileToTypedFile(conf.liveDirectory, file, typedPathCompanions))
        .flatMap(readObjects(readers, conf.liveDirectory, _))
        .map(_.toVector)
    checkedFileBased map (o ⇒ ReplaceRepo(versionId, o map (x ⇒ fileBasedSigner.sign(x withVersion versionId))))
  }

  private def retryLoginDurations: Iterator[FiniteDuration] =
    firstRetryLoginDurations.iterator ++ Iterator.continually(firstRetryLoginDurations.lastOption getOrElse 10.seconds)
}

object Provider
{
  private val typedPathCompanions = Set(AgentPath, WorkflowPath)
  private val logger = Logger(getClass)
  private val readers = AgentReader :: WorkflowReader :: Nil

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
        .guarantee(
          Task(logger.debug("logout")) >>
            provider.closeTask map (_ => logger.debug("logout completed")))

  private def logThrowable(throwable: Throwable): Unit = {
    logger.error(throwable.toStringWithCauses)
    logger.debug(throwable.toString, throwable)
  }
}
