package js7.controller.item

import js7.base.auth.{SimpleUser, UpdateItemPermission, ValidUserPermission}
import js7.base.crypt.SignedString
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.crypt.VersionedItemVerifier.Verified
import js7.data.item.ItemOperation.{AddVersion, SimpleAddOrChange, SimpleDelete, VersionedAddOrChange, VersionedDelete}
import js7.data.item.{ItemOperation, ItemPath, SimpleItem, SimpleItemId, VersionId, VersionedItem}
import monix.eval.Task
import monix.reactive.Observable

final case class VerifiedUpdateItems(
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
    val simpleItems = Vector.newBuilder[SimpleItem]
    val simpleDeletes = Vector.newBuilder[SimpleItemId]
    val versionedItems = Vector.newBuilder[Verified[VersionedItem]]
    val versionedDeletes = Vector.newBuilder[ItemPath]
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
        case SimpleAddOrChange(item) => simpleItems += item
        case SimpleDelete(itemId) => simpleDeletes += itemId
        case verifiedItem: Verified[VersionedItem] @unchecked => versionedItems += verifiedItem
        case () => assert(problemOccurred.nonEmpty)
        case VersionedDelete(path) => versionedDeletes += path
        case AddVersion(v) =>
          if (maybeVersionId.isEmpty) maybeVersionId = Some(v)
          else problemOccurred = Some(Problem("Duplicate AddVersion"))
      }
      .map { _ =>
        problemOccurred.map(Left.apply).getOrElse(
          ((maybeVersionId, versionedItems.result(), versionedDeletes.result()) match {
            case (Some(v), items, delete) => Right(Some(VerifiedUpdateItems.Versioned(v, items, delete)))
            case (None, Seq(), Seq()) => Right(None)
            case (None, _, _) => Left(Problem.pure(s"Missing VersionId"))
          }).map(maybeVersioned =>
            VerifiedUpdateItems(
              VerifiedUpdateItems.Simple(simpleItems.result(), simpleDeletes.result()),
              maybeVersioned)))
      }
      .onErrorRecover { case ExitStreamException(problem) => Left(problem) }
  }

  private case class ExitStreamException(problem: Problem) extends Exception
}
