package com.sos.jobscheduler.master.repo

import com.sos.jobscheduler.base.auth.UpdateRepoPermission
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.core.crypt.generic.GenericSignatureVerifier
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.filebased.{FileBased, RepoEvent}
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.{MasterCommand, MasterFileBaseds}
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoCommandExecutor(masterConfiguration: MasterConfiguration, signatureVerifier: GenericSignatureVerifier)
{
  private val verifier = new SignedRepoObjectVerifier[FileBased](signatureVerifier)(MasterFileBaseds.jsonCodec)

  def replaceRepoCommandToEvents(repo: Repo, replaceRepo: MasterCommand.ReplaceRepo, meta: CommandMeta): Checked[Seq[RepoEvent]] =
    meta.user.checkPermission(UpdateRepoPermission)
      .flatMap { _ =>
        val MasterCommand.ReplaceRepo(versionId, objects) = replaceRepo
        for {
          fileBaseds ← verifier.verifyAndDecodeSeq(objects).map(_.map(_._1)/*ignore senders ???*/)
          deleted = repo.currentFileBaseds.map(_.path).toSet -- fileBaseds.map(_.path).toSet
          events ← repo.fileBasedToEvents(versionId, fileBaseds, deleted)
        } yield
          events
      }

  def commandToEvents(repo: Repo, updateRepo: MasterCommand.UpdateRepo, meta: CommandMeta): Checked[Seq[RepoEvent]] =
    meta.user.checkPermission(UpdateRepoPermission)
      .flatMap { _ =>
        val MasterCommand.UpdateRepo(versionId, changedObjects, deletedPaths) = updateRepo
        for {
          fileBaseds ← verifier.verifyAndDecodeSeq(changedObjects).map(_.map(_._1)/*ignore senders ???*/)
          events ← repo.fileBasedToEvents(versionId, fileBaseds, deletedPaths)
        } yield
          events
      }
}
