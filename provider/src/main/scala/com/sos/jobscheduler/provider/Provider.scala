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
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.filebased.FileBasedReader.readObjects
import com.sos.jobscheduler.core.filebased.{FileBasedSigner, FileBaseds, TypedPathDirectoryWalker}
import com.sos.jobscheduler.core.signature.PGPSigner.readSecretKey
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAddedOrChanged, FileBasedDeleted, FileBasedEvent}
import com.sos.jobscheduler.data.filebased.{FileBased, TypedPath, VersionId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.MasterRepoReader
import com.sos.jobscheduler.master.agent.AgentReader
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.master.data.MasterCommand.UpdateRepo
import com.sos.jobscheduler.master.data.{MasterCommand, MasterFileBaseds}
import com.sos.jobscheduler.master.workflow.WorkflowReader
import com.sos.jobscheduler.provider.Provider._
import com.sos.jobscheduler.provider.configuration.ProviderConfiguration
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.immutable.{Iterable, Seq}
import scala.concurrent.duration._

// Test in com.sos.jobscheduler.tests.provider.ProviderTest
/**
  * @author Joacim Zschimmer
  */
final class Provider(conf: ProviderConfiguration)(implicit scheduler: Scheduler)
extends AutoCloseable
{
  private val api = new AkkaHttpMasterApi(conf.masterUri, config = conf.config)
  private val userAndPassword = for {
      userName ← conf.config.optionAs[String]("jobscheduler.provider.master.user")
      password ← conf.config.optionAs[String]("jobscheduler.provider.master.password")
    } yield UserAndPassword(UserId(userName), SecretString(password))
  private val repoReader = new MasterRepoReader(conf.liveDirectory)
  private val signer = new FileBasedSigner(
    MasterFileBaseds.jsonCodec,
    readSecretKey(fileAsResource(conf.configDirectory / "private" / "private-pgp-key.asc")),
    password = SecretString(conf.config.getString("jobscheduler.provider.pgp.password")))

  private var lastEntries = readDirectory

  def close() = api.logout().guarantee(Task(api.close())).runAsyncAndForget

  def updateMaster(versionId: Option[VersionId] = None): Task[Checked[MasterCommand.Response.Accepted]] =
    for {
      _ ← loginUntilReachable
      currentEntries = readDirectory
      checkedCommand = toUpdateRepoCommand(versionId, PathSeqDiffer.diff(currentEntries, lastEntries))
      //checkedCommand ← api.workflows.map(_.value).map(directoryToCommand(versionId, _))
      response ← checkedCommand.traverse(o ⇒ AkkaHttpClient.liftProblem(api.executeCommand(o))) map (_.flatten)
    } yield {
      if (response.isValid) {
        lastEntries = currentEntries
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

  private def readDirectory = DirectoryReader.entries(conf.liveDirectory)

  private def toUpdateRepoCommand(versionId: Option[VersionId], diff: PathSeqDiff): Checked[UpdateRepo] = {
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
    (checkedChanged, checkedDeleted) mapN ((chg, del) ⇒ UpdateRepo(chg map signer.sign, del, versionId))
  }

  private def directoryToCommand(versionId: Option[VersionId], current: Seq[FileBased]): Checked[UpdateRepo] =
    repoReader.readDirectoryTree()
      .map(FileBaseds.diffFileBaseds(_, current))
      .map(events ⇒ eventsToCommand(signer, versionId, events))
}

object Provider
{
  private def retryLoginDurations = Iterator.continually(10.seconds)
  private val typedPathCompanions = Set(AgentPath, WorkflowPath)
  private val logger = Logger(getClass)
  private val readers = AgentReader :: WorkflowReader :: Nil

  private[provider] def logThrowable(throwable: Throwable): Unit = {
    logger.error(throwable.toStringWithCauses)
    logger.debug(throwable.toString, throwable)
  }

  private def eventsToCommand(signer: FileBasedSigner, versionId: Option[VersionId], events: Iterable[FileBasedEvent]): UpdateRepo =
    UpdateRepo(
      events.collect { case e: FileBasedAddedOrChanged ⇒ signer.sign(e.fileBased) } .toVector,
      events.collect { case e: FileBasedDeleted ⇒ e.path } .toVector,
      versionId)
}
