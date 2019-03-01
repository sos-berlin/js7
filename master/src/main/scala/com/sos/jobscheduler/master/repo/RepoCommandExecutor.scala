package com.sos.jobscheduler.master.repo

import com.sos.jobscheduler.base.auth.UpdateRepoPermission
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.core.filebased.{FileBasedVerifier, Repo}
import com.sos.jobscheduler.data.crypt.{Signed, SignedString}
import com.sos.jobscheduler.data.filebased.{FileBased, RepoEvent}
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.repo.RepoCommandExecutor._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class RepoCommandExecutor(fileBasedVerifier: FileBasedVerifier[FileBased])
{
  // ReplaceRepo and UpdateRepo may detect equal objects and optimize the FileBasedChanged away,
  // if we can make sure that the different signature (due to different VersionId) refer the same trusted signer key.
  // Signatures refering different signer keys must be kept to allow the operator to delete old signer keys.

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

  def updateRepoCommandToEvents(repo: Repo, updateRepo: MasterCommand.UpdateRepo, meta: CommandMeta): Checked[Seq[RepoEvent]] =
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
      logger.info(verified.toString)
      verified.signedFileBased
    }
}

object RepoCommandExecutor {
  private val logger = Logger(getClass)
}
