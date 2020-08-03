package js7.controller.repo

import cats.instances.either._
import cats.instances.vector._
import cats.syntax.traverse._
import js7.base.auth.UpdateRepoPermission
import js7.base.crypt.{Signed, SignedString}
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits._
import js7.common.scalautil.Logger
import js7.controller.data.ControllerCommand
import js7.controller.repo.RepoCommandExecutor._
import js7.core.command.CommandMeta
import js7.data.crypt.FileBasedVerifier
import js7.data.filebased.{FileBased, Repo, RepoEvent}
import monix.eval.Task
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final class RepoCommandExecutor(fileBasedVerifier: FileBasedVerifier[FileBased])
{
  // ReplaceRepo and UpdateRepo may detect equal objects and optimize the FileBasedChanged away,
  // if we can make sure that the different signature (due to different VersionId) refer the same trusted signer key.
  // Signatures refering different signer keys must be kept to allow the operator to delete old signer keys.

  def replaceRepoCommandToEvents(repo: Repo, replaceRepo: ControllerCommand.ReplaceRepo, meta: CommandMeta): Task[Checked[Seq[RepoEvent]]] =
    Task(meta.user.checkPermission(UpdateRepoPermission))
      .flatMapF(_ =>
        Observable.fromIterable(replaceRepo.objects)
          .mapParallelOrdered(sys.runtime.availableProcessors)(o => Task(verify(o)))
          .toL(Vector)
          .map(_
            .sequence
            .flatMap(signedFileBasedSeq => repo.fileBasedToEvents(replaceRepo.versionId, signedFileBasedSeq,
              deleted = repo.currentFileBaseds.view
                .map(_.path)
                .filterNot(signedFileBasedSeq.view.map(_.value.path).toSet)
                .to(Vector)))))

  def updateRepoCommandToEvents(repo: Repo, updateRepo: ControllerCommand.UpdateRepo, meta: CommandMeta): Task[Checked[Seq[RepoEvent]]] =
    Task(meta.user.checkPermission(UpdateRepoPermission))
      .flatMapF(_ =>
        Observable.fromIterable(updateRepo.change)
          .mapParallelOrdered(sys.runtime.availableProcessors)(o => Task(verify(o)))
          .toL(Vector)
          .map(_
            .sequence
            .flatMap(repo.fileBasedToEvents(updateRepo.versionId, _, updateRepo.delete))))

  private def verify(signedString: SignedString): Checked[Signed[FileBased]] =
    for (verified <- fileBasedVerifier.verify(signedString)) yield {
      logger.info(Logger.Signature, verified.toString)
      verified.signedFileBased
    }
}

object RepoCommandExecutor {
  private val logger = Logger(getClass)
}
