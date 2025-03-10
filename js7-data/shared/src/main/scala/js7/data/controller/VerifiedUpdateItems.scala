package js7.data.controller

import cats.effect.IO
import fs2.Stream
import js7.base.auth.{SimpleUser, UpdateItemPermission, ValidUserPermission}
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.crypt.SignedString
import js7.base.fs2utils.StreamExtensions.mapParallelBatch
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.crypt.SignedItemVerifier.Verified
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.{InventoryItemKey, ItemOperation, SignableItem, SignableSimpleItem, SimpleItemPath, UnsignedSimpleItem, VersionId, VersionedItem, VersionedItemPath}
import scala.collection.View

final case class VerifiedUpdateItems private[controller](
  simple: VerifiedUpdateItems.Simple,
  maybeVersioned: Option[VerifiedUpdateItems.Versioned] = None):

  def itemCount: Int =
    simple.itemCount + maybeVersioned.fold(0)(_.verifiedItems.size)

  def addOrChangeKeys: View[InventoryItemKey] =
    simple.unsignedSimpleItems.view.map(_.key) ++
      simple.verifiedSimpleItems.view.map(_.item.key) ++
      maybeVersioned.view.flatMap(_.verifiedItems.view.map(_.item.id))


object VerifiedUpdateItems:
  final case class Simple(
    unsignedSimpleItems: Seq[UnsignedSimpleItem] = Nil,
    verifiedSimpleItems: Seq[Verified[SignableSimpleItem]] = Nil,
    delete: Seq[SimpleItemPath] = Nil):
    def itemCount: Int =
      unsignedSimpleItems.size + verifiedSimpleItems.size

  final case class Versioned(
    versionId: VersionId,
    verifiedItems: Seq[Verified[VersionedItem]] = Nil,
    remove: Seq[VersionedItemPath] = Nil):
    private[VerifiedUpdateItems] def paths: View[VersionedItemPath] =
      verifiedItems.view.map(_.item.path) ++ remove

  def fromOperations(
    stream: Stream[IO, ItemOperation],
    verify: SignedString => Checked[Verified[SignableItem]],
    user: SimpleUser)
  : IO[Checked[VerifiedUpdateItems]] =
    IO(user.checkPermissions(ValidUserPermission, UpdateItemPermission))
      .flatMapT(_ => fromOperationsOnly(stream, verify))

  private def fromOperationsOnly(
    stream: Stream[IO, ItemOperation],
    verify: SignedString => Checked[Verified[SignableItem]])
  : IO[Checked[VerifiedUpdateItems]] =
    val unsignedSimpleItems_ = Vector.newBuilder[UnsignedSimpleItem]
    val signedItems_ = Vector.newBuilder[Verified[SignableItem]]
    val simpleDeletes_ = Vector.newBuilder[SimpleItemPath]
    val versionedRemoves_ = Vector.newBuilder[VersionedItemPath]
    @volatile var maybeVersionId: Option[VersionId] = None
    @volatile var problemOccurred: Option[Problem] = None

    stream
      .mapParallelBatch():
        case AddOrChangeSigned(signedString) =>
          if problemOccurred.isEmpty then
            verify(signedString) match
              case Left(problem) =>
                problemOccurred = Some(problem)
                ()  // Delay error until input stream is completely eaten
              case Right(verified) =>
                verified
          else
            ()

        case o => o
      .foreach(o => IO(o match
          case AddOrChangeSimple(item) => unsignedSimpleItems_ += item
          case DeleteSimple(path) => simpleDeletes_ += path
          case verifiedItem: Verified[SignableItem] @unchecked => signedItems_ += verifiedItem
          case () => assert(problemOccurred.nonEmpty)
          case RemoveVersioned(path) => versionedRemoves_ += path
          case AddVersion(v) =>
            if maybeVersionId.isEmpty then maybeVersionId = Some(v)
            else problemOccurred = Some(Problem("Duplicate AddVersion"))
          // To satisfy the Scala compiler:
          case AddOrChangeSigned(_) => throw new IllegalStateException))
      .compile
      .drain
      .map: _ =>
        problemOccurred.map(Left.apply).getOrElse:
          val unsignedSimpleItems = unsignedSimpleItems_.result()
          val signedItems = signedItems_.result()
          val versionedItems = signedItems.flatMap(_.ifCast[VersionedItem])
          val signedSimpleItems = signedItems.flatMap(_.ifCast[SignableSimpleItem])
          val simpleDeletes = simpleDeletes_.result()

          unsignedSimpleItems
            .view
            .map(_.key)
            .concat(simpleDeletes)
            .checkUniquenessBy(identity)
            .flatMap(_ =>
              checkVersioned(maybeVersionId, versionedItems, versionedRemoves_.result()))
            .map(maybeVersioned =>
              VerifiedUpdateItems(
                VerifiedUpdateItems.Simple(unsignedSimpleItems, signedSimpleItems, simpleDeletes),
                maybeVersioned))
      .recoverWith:
        case ExitStreamException(problem) => IO.left(problem)

  private def checkVersioned(
    maybeVersionId: Option[VersionId],
    verifiedVersionedItems: Seq[Verified[VersionedItem]],
    versionedRemoves: Seq[VersionedItemPath]
  ): Checked[Option[Versioned]] =
    (maybeVersionId, verifiedVersionedItems, versionedRemoves) match
      case (Some(v), verifiedVersionedItems, remove) =>
        for _ <- (verifiedVersionedItems.view.map(_.item.path) ++ remove).checkUniquenessBy(identity) yield
          Some(VerifiedUpdateItems.Versioned(v, verifiedVersionedItems, remove))
      case (None, Seq(), Seq()) =>
        Right(None)
      case (None, _, _) =>
        Left(Problem.pure("VersionedItem but AddVersionId operation is missing"))

  private case class ExitStreamException(problem: Problem) extends Exception
