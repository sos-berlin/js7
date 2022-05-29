package js7.data.controller

import js7.base.auth.{SimpleUser, UpdateItemPermission, ValidUserPermission}
import js7.base.crypt.SignedString
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.crypt.SignedItemVerifier.Verified
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.{InventoryItemKey, ItemOperation, SignableItem, SignableSimpleItem, SimpleItemPath, UnsignedSimpleItem, VersionId, VersionedItem, VersionedItemPath}
import monix.eval.Task
import monix.reactive.Observable
import scala.collection.View

final case class VerifiedUpdateItems private(
  simple: VerifiedUpdateItems.Simple,
  maybeVersioned: Option[VerifiedUpdateItems.Versioned] = None)
{
  def itemCount =
    simple.itemCount + maybeVersioned.fold(0)(_.verifiedItems.size)

  def versionedPaths: View[VersionedItemPath] =
    maybeVersioned.view.flatMap(_.paths)

  def addOrChangeKeys: View[InventoryItemKey] =
    simple.unsignedSimpleItems.view.map(_.key) ++
      simple.verifiedSimpleItems.view.map(_.item.key) ++
      maybeVersioned.view.flatMap(_.verifiedItems.view.map(_.item.id))

  def addedOrChangedAgentPaths: View[AgentPath] =
    simple.unsignedSimpleItems.view.collect { case o: AgentRef => o.path }
}

object VerifiedUpdateItems
{
  final case class Simple(
    unsignedSimpleItems: Seq[UnsignedSimpleItem] = Nil,
    verifiedSimpleItems: Seq[Verified[SignableSimpleItem]] = Nil,
    delete: Seq[SimpleItemPath] = Nil)
  {
    def itemCount =
      unsignedSimpleItems.size + verifiedSimpleItems.size
  }

  final case class Versioned(
    versionId: VersionId,
    verifiedItems: Seq[Verified[VersionedItem]] = Nil,
    remove: Seq[VersionedItemPath] = Nil)
  {
    private[VerifiedUpdateItems] def paths: View[VersionedItemPath] =
      verifiedItems.view.map(_.item.path) ++ remove
  }

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
    val simpleDeletes_ = Vector.newBuilder[SimpleItemPath]
    val versionedRemoves_ = Vector.newBuilder[VersionedItemPath]
    @volatile var maybeVersionId: Option[VersionId] = None
    @volatile var problemOccurred: Option[Problem] = None

    observable
      .mapParallelBatch() {
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
        case DeleteSimple(path) => simpleDeletes_ += path
        case verifiedItem: Verified[SignableItem] @unchecked => signedItems_ += verifiedItem
        case () => assert(problemOccurred.nonEmpty)
        case RemoveVersioned(path) => versionedRemoves_ += path
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
            .map(_.key)
            .concat(simpleDeletes)
            .checkUniqueness(identity)
            .flatMap(_ =>
              checkVersioned(maybeVersionId, versionedItems, versionedRemoves_.result()))
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
    versionedRemoves: Seq[VersionedItemPath]
  ): Checked[Option[Versioned]] =
    (maybeVersionId, verifiedVersionedItems, versionedRemoves) match {
      case (Some(v), verifiedVersionedItems, remove) =>
        for (_ <- (verifiedVersionedItems.view.map(_.item.path) ++ remove).checkUniqueness(identity)) yield
          Some(VerifiedUpdateItems.Versioned(v, verifiedVersionedItems, remove))
      case (None, Seq(), Seq()) =>
        Right(None)
      case (None, _, _) =>
        Left(Problem.pure(s"VersionedItem but AddVersionId operation is missing"))
    }

  private case class ExitStreamException(problem: Problem) extends Exception
}
