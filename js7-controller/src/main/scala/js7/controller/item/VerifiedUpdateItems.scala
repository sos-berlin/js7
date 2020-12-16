package js7.controller.item

import js7.base.auth.{SimpleUser, UpdateItemPermission, ValidUserPermission}
import js7.base.crypt.SignedString
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichTraversable
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.crypt.VersionedItemVerifier.Verified
import js7.data.item.ItemOperation.{AddVersion, SimpleAddOrChange, SimpleDelete, VersionedAddOrChange, VersionedDelete}
import js7.data.item.{ItemOperation, ItemPath, SimpleItem, SimpleItemId, VersionId, VersionedItem}
import monix.eval.Task
import monix.reactive.Observable

final case class VerifiedUpdateItems private[item](
  simple: VerifiedUpdateItems.Simple,
  maybeVersioned: Option[VerifiedUpdateItems.Versioned])

object VerifiedUpdateItems
{
  final case class Simple(
    items: Seq[SimpleItem],
    delete: Seq[SimpleItemId])

  final case class Versioned(
    versionId: VersionId,
    verifiedItems: Seq[Verified[VersionedItem]],
    delete: Seq[ItemPath])

  def fromOperations(
    observable: Observable[ItemOperation],
    verify: SignedString => Checked[Verified[VersionedItem]],
    user: SimpleUser)
  : Task[Checked[VerifiedUpdateItems]] =
    Task(user.checkPermissions(ValidUserPermission, UpdateItemPermission))
      .flatMapT(_ => fromOperationsOnly(observable, verify))

  private def fromOperationsOnly(
    observable: Observable[ItemOperation],
    verify: SignedString => Checked[Verified[VersionedItem]])
  : Task[Checked[VerifiedUpdateItems]] = {
    val simpleItems_ = Vector.newBuilder[SimpleItem]
    val simpleDeletes_ = Vector.newBuilder[SimpleItemId]
    val versionedItems_ = Vector.newBuilder[Verified[VersionedItem]]
    val versionedDeletes_ = Vector.newBuilder[ItemPath]
    @volatile var maybeVersionId: Option[VersionId] = None
    @volatile var problemOccurred: Option[Problem] = None

    observable
      .mapParallelUnorderedBatch() {
          case VersionedAddOrChange(signedJson) =>
            if (problemOccurred.isEmpty) {
              verify(signedJson) match {
                case Left(problem) =>
                  problemOccurred = Some(problem)
                  ()  // Delay error until input stream is completely eaten
                case Right(verified) => verified
              }
            } else
              ()
          case o => o
        }
      .foreachL {
        case SimpleAddOrChange(item) => simpleItems_ += item
        case SimpleDelete(itemId) => simpleDeletes_ += itemId
        case verifiedItem: Verified[VersionedItem] @unchecked => versionedItems_ += verifiedItem
        case () => assert(problemOccurred.nonEmpty)
        case VersionedDelete(path) => versionedDeletes_ += path
        case AddVersion(v) =>
          if (maybeVersionId.isEmpty) maybeVersionId = Some(v)
          else problemOccurred = Some(Problem("Duplicate AddVersion"))
      }
      .map { _ =>
        problemOccurred.map(Left.apply).getOrElse {
          val simpleItems = simpleItems_.result()
          val simpleDeletes = simpleDeletes_.result()
          (simpleItems.view.map(_.id) ++ simpleDeletes).checkUniqueness(identity)
            .flatMap(_ => checkVersioned(maybeVersionId, versionedItems_.result(), versionedDeletes_.result()))
            .map(maybeVersioned =>
              VerifiedUpdateItems(
                VerifiedUpdateItems.Simple(simpleItems, simpleDeletes),
                maybeVersioned))
        }
      }
      .onErrorRecover { case ExitStreamException(problem) => Left(problem) }
  }

  private def checkVersioned(
    maybeVersionId: Option[VersionId],
    verifiedVersionedItems: Seq[Verified[VersionedItem]],
    versionedDeletes: Seq[ItemPath]
  ): Checked[Option[Versioned]] =
    (maybeVersionId, verifiedVersionedItems, versionedDeletes) match {
      case (Some(v), verifiedVersionedItems, delete) =>
        for (_ <- (verifiedVersionedItems.view.map(_.item.path) ++ delete).checkUniqueness(identity)) yield
          Some(VerifiedUpdateItems.Versioned(v, verifiedVersionedItems, delete))
      case (None, Seq(), Seq()) =>
        Right(None)
      case (None, _, _) =>
        Left(Problem.pure("Missing VersionId"))
    }

  private case class ExitStreamException(problem: Problem) extends Exception
}
