package com.sos.jobscheduler.master.repo

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.auth.UpdateRepoPermission
import com.sos.jobscheduler.base.convert.AsJava._
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.JavaSyncResources.fileAsResource
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.core.problems.PGPMessageSignedByUnknownProblem
import com.sos.jobscheduler.core.signature.PGPCommons.readPublicKeyRingCollection
import com.sos.jobscheduler.core.signature.PGPSignatureVerifier
import com.sos.jobscheduler.data.filebased.{FileBased, RepoEvent}
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.{MasterCommand, MasterFileBaseds}
import com.sos.jobscheduler.master.repo.UpdateRepoCommandExecutor._
import java.nio.file.Files.exists
import java.nio.file.Path
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoCommandExecutor(masterConfiguration: MasterConfiguration)
{
  private val checkedSignedRepoObjectVerifier = {
    val trustedKeysFile = masterConfiguration.config.optionAs[Path]("jobscheduler.trust.pgp-keys-file")  // Good name ???
      .getOrElse(masterConfiguration.configDirectory / defaultTrustedKeyFile)
    if (!exists(trustedKeysFile)) {
      logger.warn(s"Command UpdateRepo will be rejected due to missing file '$trustedKeysFile'")
      Invalid(PGPMessageSignedByUnknownProblem)  // Same message when signer is unknown
    } else {
      val signatureVerifier = new PGPSignatureVerifier(readPublicKeyRingCollection(fileAsResource(trustedKeysFile)))
      Valid(new SignedRepoObjectVerifier[FileBased](signatureVerifier)(MasterFileBaseds.jsonCodec))
    }
  }

  def replaceRepoCommandToEvents(repo: Repo, replaceRepo: MasterCommand.ReplaceRepo, meta: CommandMeta): Checked[Seq[RepoEvent]] =
    meta.user.checkPermission(UpdateRepoPermission)
      .flatMap(_ ⇒ checkedSignedRepoObjectVerifier)
      .flatMap { verifier ⇒
        val MasterCommand.ReplaceRepo(objects, versionIdOption) = replaceRepo
        val versionId = versionIdOption getOrElse repo.newVersionId()
        for {
          fileBaseds ← verifier.verifyAndDecodeSeq(objects).map(_.map(_._1)/*ignore senders ???*/)
          deleted = repo.currentFileBaseds.map(_.path).toSet -- fileBaseds.map(_.path).toSet
          events ← repo.fileBasedToEvents(versionId, fileBaseds, deleted)
        } yield
          events
      }

  def commandToEvents(repo: Repo, updateRepo: MasterCommand.UpdateRepo, meta: CommandMeta): Checked[Seq[RepoEvent]] =
    meta.user.checkPermission(UpdateRepoPermission)
      .flatMap(_ ⇒ checkedSignedRepoObjectVerifier)
      .flatMap { verifier ⇒
        val MasterCommand.UpdateRepo(changedObjects, deletedPaths, versionIdOption) = updateRepo
        val versionId = versionIdOption getOrElse repo.newVersionId()
        for {
          fileBaseds ← verifier.verifyAndDecodeSeq(changedObjects).map(_.map(_._1)/*ignore senders ???*/)
          events ← repo.fileBasedToEvents(versionId, fileBaseds, deletedPaths)
        } yield
          events
      }
}

object UpdateRepoCommandExecutor {
  private val logger = Logger(getClass)
  private val defaultTrustedKeyFile = "private/trusted-pgp-keys.asc"
}
