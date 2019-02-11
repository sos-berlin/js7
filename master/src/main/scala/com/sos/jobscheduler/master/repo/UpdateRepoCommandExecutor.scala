package com.sos.jobscheduler.master.repo

import com.sos.jobscheduler.base.auth.UpdateRepoPermission
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.core.filebased.{FileBasedVerifier, Repo}
import com.sos.jobscheduler.data.crypt.{Signed, SignedString}
import com.sos.jobscheduler.data.filebased.{FileBased, RepoEvent}
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.repo.UpdateRepoCommandExecutor._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoCommandExecutor(masterConfiguration: MasterConfiguration, fileBasedVerifier: FileBasedVerifier)
{
  def replaceRepoCommandToEvents(repo: Repo, replaceRepo: MasterCommand.ReplaceRepo, meta: CommandMeta): Checked[Seq[RepoEvent]] =
    meta.user.checkPermission(UpdateRepoPermission)
      .flatMap { _ =>
        val MasterCommand.ReplaceRepo(versionId, objects) = replaceRepo
        for {
          signedFileBaseds <- objects.failFastMap(verify)
          deleted = repo.currentFileBaseds.map(_.path).toSet -- signedFileBaseds.map(_.value.path).toSet
          events <- repo.fileBasedToEvents(versionId, signedFileBaseds, deleted)
        } yield
          events
      }

  def commandToEvents(repo: Repo, updateRepo: MasterCommand.UpdateRepo, meta: CommandMeta): Checked[Seq[RepoEvent]] =
    meta.user.checkPermission(UpdateRepoPermission)
      .flatMap { _ =>
        val MasterCommand.UpdateRepo(versionId, changedObjects, deletedPaths) = updateRepo
        for {
          signedFileBaseds <- changedObjects.failFastMap(verify)
          events <- repo.fileBasedToEvents(versionId, signedFileBaseds, deletedPaths)
        } yield
          events
      }

  private def verify(signedString: SignedString): Checked[Signed[FileBased]] =
    for (verified <- fileBasedVerifier.verify(signedString)) yield {
      logger.info(s"Configuration object '${verified.fileBased.id}' verified, signed by ${verified.signerIds.mkString("'", "', '", "'")}")
      verified.signedFileBased
    }
}

object UpdateRepoCommandExecutor {
  private val logger = Logger(getClass)
}
