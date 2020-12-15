package js7.controller.item

import cats.instances.either._
import cats.instances.vector._
import cats.syntax.traverse._
import js7.base.auth.UpdateItemPermission
import js7.base.crypt.{Signed, SignedString}
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.common.scalautil.Logger
import js7.controller.data.ControllerCommand
import js7.controller.item.ItemCommandExecutor._
import js7.core.command.CommandMeta
import js7.data.crypt.VersionedItemVerifier
import js7.data.item.{Repo, VersionedEvent, VersionedItem}
import monix.eval.Task
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
final class ItemCommandExecutor(itemVerifier: VersionedItemVerifier[VersionedItem])
{
  // ReplaceRepo and UpdateRepo may detect equal objects and optimize the VersionedItemChanged away,
  // if we can make sure that the different signature (due to different VersionId) refer the same trusted signer key.
  // Signatures refering different signer keys must be kept to allow the operator to delete old signer keys.

  def replaceRepoCommandToEvents(repo: Repo, replaceRepo: ControllerCommand.ReplaceRepo, meta: CommandMeta): Task[Checked[Seq[VersionedEvent]]] =
    Task(meta.user.checkPermission(UpdateItemPermission))
      .flatMapT(_ =>
        Observable.fromIterable(replaceRepo.objects)
          .mapParallelOrdered(sys.runtime.availableProcessors)(o => Task(verify(o)))
          .toL(Vector)
          .map(_
            .sequence
            .flatMap(signedItemSeq => repo.itemsToEvents(replaceRepo.versionId, signedItemSeq,
              deleted = repo.currentItems.view
                .map(_.path)
                .filterNot(signedItemSeq.view.map(_.value.path).toSet)
                .to(Vector)))))

  def updateRepoCommandToEvents(repo: Repo, updateRepo: ControllerCommand.UpdateRepo, meta: CommandMeta): Task[Checked[Seq[VersionedEvent]]] =
    Task(meta.user.checkPermission(UpdateItemPermission))
      .flatMapT(_ =>
        Observable.fromIterable(updateRepo.change)
          .mapParallelOrdered(sys.runtime.availableProcessors)(o => Task(verify(o)))
          .toL(Vector)
          .map(_
            .sequence
            .flatMap(repo.itemsToEvents(updateRepo.versionId, _, updateRepo.delete))))

  private def verify(signedString: SignedString): Checked[Signed[VersionedItem]] =
    for (verified <- itemVerifier.verify(signedString)) yield {
      logger.info(Logger.SignatureVerified, verified.toString)
      verified.signedItem
    }
}

object ItemCommandExecutor {
  private val logger = Logger(getClass)
}
