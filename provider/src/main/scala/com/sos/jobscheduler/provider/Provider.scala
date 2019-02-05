package com.sos.jobscheduler.provider

import cats.instances.vector._
import cats.syntax.apply._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.convert.As._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.files.{DirectoryReader, PathSeqDiff, PathSeqDiffer}
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.JavaSyncResources.fileAsResource
import com.sos.jobscheduler.common.scalautil.MonixUtils.autoCloseableToObservable
import com.sos.jobscheduler.common.scalautil.{IOExecutor, Logger}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.crypt.pgp.PgpSigner.readSecretKey
import com.sos.jobscheduler.core.filebased.FileBasedReader.readObjects
import com.sos.jobscheduler.core.filebased.{FileBasedSigner, TypedPathDirectoryWalker}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.{FileBased, TypedPath, VersionId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.agent.AgentReader
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.master.data.MasterCommand.{ReplaceRepo, UpdateRepo}
import com.sos.jobscheduler.master.data.{MasterCommand, MasterFileBaseds}
import com.sos.jobscheduler.master.workflow.WorkflowReader
import com.sos.jobscheduler.provider.Provider._
import com.sos.jobscheduler.provider.configuration.ProviderConfiguration
import java.nio.file.Path
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
final class Provider(fileBasedSigner: FileBasedSigner, val conf: ProviderConfiguration)(implicit scheduler: Scheduler)
extends AutoCloseable with Observing
{
  private val api = new AkkaHttpMasterApi(conf.masterUri, config = conf.config)
  private val userAndPassword = for {
      userName ← conf.config.optionAs[String]("jobscheduler.provider.master.user")
      password ← conf.config.optionAs[String]("jobscheduler.provider.master.password")
    } yield UserAndPassword(UserId(userName), SecretString(password))
  private val firstRetryLoginDurations = conf.config.getDurationList("jobscheduler.provider.master.login-retry-delays")
    .asScala.map(_.toFiniteDuration)

  private val lastEntries = AtomicAny(Vector.empty[DirectoryReader.Entry])

  def close() = api.logout().guarantee(Task(api.close())).runAsyncAndForget

  def replaceMasterConfiguration(versionId: VersionId): Task[Checked[MasterCommand.Response.Accepted]] =
    for {
      _ ← loginUntilReachable
      currentEntries = readDirectory
      checkedCommand = toReplaceRepoCommand(versionId, currentEntries map (_.path))
      response ← checkedCommand.traverse(o ⇒ AkkaHttpClient.liftProblem(api.executeCommand(o))) map (_.flatten)
    } yield {
      if (response.isValid) {
        lastEntries := currentEntries
      }
      response
    }

  def updateMasterConfiguration(versionId: VersionId): Task[Checked[Unit]] =
    for {
      _ ← loginUntilReachable
      currentEntries = readDirectory
      last = lastEntries.get
      checkedCommand = toUpdateRepoCommand(versionId, PathSeqDiffer.diff(currentEntries, last))
      response ← checkedCommand.traverse(executeUpdateRepo) map (_.flatten)
    } yield {
      if (response.isValid) {
        if (!lastEntries.compareAndSet(last, currentEntries)) throw new ConcurrentModificationException("Provider has been concurrently used")
      }
      response
    }

  private def executeUpdateRepo(updateRepo: UpdateRepo) =
    if (updateRepo.isEmpty ) Task.pure(Checked.unit)
    else AkkaHttpClient.liftProblem(api.executeCommand(updateRepo) map (_ ⇒ ()))

  private lazy val loginUntilReachable: Task[Unit] =
    Task {
      if (api.hasSession)
        Task.unit
      else
        api.loginUntilReachable(userAndPassword, retryLoginDurations, logThrowable).map(_ ⇒ ())
    }.flatten

  private def readDirectory = DirectoryReader.entries(conf.liveDirectory)

  private def toUpdateRepoCommand(versionId: VersionId, diff: PathSeqDiff): Checked[UpdateRepo] = {
    val checkedChanged: Checked[Vector[FileBased]] =
      (diff.added ++ diff.changed)
        .map(path ⇒ TypedPathDirectoryWalker.fileToTypedFile(conf.liveDirectory, path, typedPathCompanions))
        .toVector.sequence
        .flatMap(readObjects(readers, conf.liveDirectory, _))
        .map(_.toVector)
    val checkedDeleted: Checked[Vector[TypedPath]] =
      diff.deleted.map(path ⇒ TypedPathDirectoryWalker.fileToTypedFile(conf.liveDirectory, path, typedPathCompanions))
        .toVector.sequence
        .map(_ map (_.path))
    (checkedChanged, checkedDeleted) mapN ((chg, del) ⇒ UpdateRepo(versionId, chg map (o ⇒ fileBasedSigner.sign(o withVersion versionId)), del))
  }

  private def toReplaceRepoCommand(versionId: VersionId, files: Seq[Path]): Checked[ReplaceRepo] = {
    val checkedFileBased: Checked[Vector[FileBased]] =
      files
        .map(file⇒ TypedPathDirectoryWalker.fileToTypedFile(conf.liveDirectory, file, typedPathCompanions))
        .toVector.sequence
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

  def apply(conf: ProviderConfiguration)(implicit s: Scheduler): Checked[Provider]  =
    FileBasedSigner(
      MasterFileBaseds.jsonCodec,
      readSecretKey(fileAsResource(conf.configDirectory / "private" / "private-pgp-key.asc")),
      password = SecretString(conf.config.getString("jobscheduler.provider.pgp.password"))
    ).map(new Provider(_, conf))


  def observe(conf: ProviderConfiguration)(implicit s: Scheduler, iox: IOExecutor): Checked[Observable[Unit]] =
    Provider(conf).map(autoCloseableToObservable(_).flatMap(_.observe))

  private[provider] def logThrowable(throwable: Throwable): Unit = {
    logger.error(throwable.toStringWithCauses)
    logger.debug(throwable.toString, throwable)
  }
}
