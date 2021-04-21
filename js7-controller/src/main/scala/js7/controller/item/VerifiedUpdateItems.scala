package js7.controller.item

import js7.base.auth.{SimpleUser, UpdateItemPermission, ValidUserPermission}
import js7.base.crypt.SignedString
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.crypt.SignedItemVerifier.Verified
import js7.data.item.ItemOperation.{AddVersion, AddOrChangeSigned, AddOrChangeSimple, DeleteSimple, DeleteVersioned}
import js7.data.item.{ItemOperation, ItemPath, SignableItem, SignableSimpleItem, SimpleItemId, UnsignedSimpleItem, VersionId, VersionedItem}
import monix.eval.Task
import monix.reactive.Observable

final case class VerifiedUpdateItems private[item](
  simple: VerifiedUpdateItems.Simple,
  maybeVersioned: Option[VerifiedUpdateItems.Versioned])
{
  def itemCount = simple.itemCount + maybeVersioned.fold(0)(_.verifiedItems.size)
}

object VerifiedUpdateItems
{
  final case class Simple(
    unsignedSimpleItems: Seq[UnsignedSimpleItem],
    verifiedSimpleItems: Seq[Verified[SignableSimpleItem]],
    delete: Seq[SimpleItemId])
  {
    def itemCount = unsignedSimpleItems.size + verifiedSimpleItems.size
  }

  final case class Versioned(
    versionId: VersionId,
    verifiedItems: Seq[Verified[VersionedItem]],
    delete: Seq[ItemPath])

  def fromOperations(
    observable: Observable[ItemOperation],
    verify: SignedString => Checked[Verified[SignableItem]],
    user: SimpleUser)
  : Task[Checked[VerifiedUpdateItems]] =
    Task(user.checkPermissions(ValidUserPermission, UpdateItemPermission))
      .flatMapT(_ => fromOperationsOnly(observable, verify))

  private def fromOperationsOnly(
    observable: Observable[ItemOperation],
    verify: SignedString => Checked[Verified[SignableItem]])
  : Task[Checked[VerifiedUpdateItems]] =
  {
    val unsignedSimpleItems_ = Vector.newBuilder[UnsignedSimpleItem]
    val signedItems_ = Vector.newBuilder[Verified[SignableItem]]
    val simpleDeletes_ = Vector.newBuilder[SimpleItemId]
    val versionedDeletes_ = Vector.newBuilder[ItemPath]
    @volatile var maybeVersionId: Option[VersionId] = None
    @volatile var problemOccurred: Option[Problem] = None

    observable
      .mapParallelUnorderedBatch() {
          case AddOrChangeSigned(signedString) =>
            if (problemOccurred.isEmpty)
              verify(signedString) match {
                case Left(problem) =>
                  problemOccurred = Some(problem)
                  ()  // Delay error until input stream is completely eaten
                case Right(verified) =>
                  verified
              }
            else
              ()

          case o => o
        }
      .foreachL {
        case AddOrChangeSimple(item) => unsignedSimpleItems_ += item
        case DeleteSimple(itemId) => simpleDeletes_ += itemId
        case verifiedItem: Verified[SignableItem] @unchecked => signedItems_ += verifiedItem
        case () => assert(problemOccurred.nonEmpty)
        case DeleteVersioned(path) => versionedDeletes_ += path
        case AddVersion(v) =>
          if (maybeVersionId.isEmpty) maybeVersionId = Some(v)
          else problemOccurred = Some(Problem("Duplicate AddVersion"))
      }
      .map { _ =>
        problemOccurred.map(Left.apply).getOrElse {
          val unsignedSimpleItems = unsignedSimpleItems_.result()
          val signedItems = signedItems_.result()
          val versionedItems = signedItems.flatMap(_.ifCast[VersionedItem])
          val signedSimpleItems = signedItems.flatMap(_.ifCast[SignableSimpleItem])
          val simpleDeletes = simpleDeletes_.result()

          unsignedSimpleItems
            .view
            .map(_.id)
            .concat(simpleDeletes)
            .checkUniqueness(identity)
            .flatMap(_ =>
              checkVersioned(maybeVersionId, versionedItems, versionedDeletes_.result()))
            .map(maybeVersioned =>
              VerifiedUpdateItems(
                VerifiedUpdateItems.Simple(unsignedSimpleItems, signedSimpleItems, simpleDeletes),
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
        Left(Problem.pure(s"VersionedItem but AddVersionId operation is missing"))
    }

  private case class ExitStreamException(problem: Problem) extends Exception
}
