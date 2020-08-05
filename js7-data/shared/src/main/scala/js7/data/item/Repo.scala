package js7.data.item

import cats.instances.either._
import cats.instances.vector._
import cats.syntax.traverse._
import js7.base.crypt.Signed
import js7.base.problem.Checked._
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections._
import js7.base.utils.Collections.implicits._
import js7.base.utils.Memoizer
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.{EventVersionDoesNotMatchProblem, ItemDeletedProblem, ItemVersionDoesNotMatchProblem}
import js7.data.crypt.InventoryItemVerifier
import js7.data.item.Repo.Entry
import js7.data.item.RepoEvent.{ItemAdded, ItemAddedOrChanged, ItemChanged, ItemDeleted, ItemEvent, VersionAdded}
import org.jetbrains.annotations.TestOnly
import scala.collection.{View, mutable}

/**
  * Representation of versioned InventoryItem (configuration objects).
  * @param versionSet `versionSet == versions.toSet`
  * @author Joacim Zschimmer
  */
final case class Repo private(
  versions: List[VersionId],
  versionSet: Set[VersionId],
  pathToVersionToSignedItems: Map[TypedPath, List[Entry]],
  itemVerifier: Option[InventoryItemVerifier[InventoryItem]])
{
  assertThat(versions.nonEmpty || pathToVersionToSignedItems.isEmpty)

  /** `itemVerifier` is not compared - for testing only. */
  override def equals(o: Any) = o match {
    case o: Repo => versions == o.versions && pathToVersionToSignedItems == o.pathToVersionToSignedItems
    case _ => false
  }

  import Repo._

  lazy val versionId: VersionId = versions.headOption getOrElse VersionId.Anonymous

  def currentVersionSize =
    currentSignedItems.size

  lazy val currentVersion: Map[TypedPath, Signed[InventoryItem]] =
    currentSignedItems.view.map(o => o.value.path -> o).toMap

  private lazy val typeToPathToCurrentItem: InventoryItem.Companion_ => Map[TypedPath, Signed[InventoryItem]] =
    Memoizer.nonStrict { companion: InventoryItem.Companion_ =>
      currentVersion collect {
        case (path, signedItem) if signedItem.value.companion == companion =>
          path -> signedItem
      }
    }

  /** Returns the difference to the repo as events. */
  def itemToEvents(versionId: VersionId, changed: Iterable[Signed[InventoryItem]], deleted: Iterable[TypedPath] = Nil)
  : Checked[Seq[RepoEvent]] =
    checkVersion(versionId, changed)
      .flatMap { changed =>
        val addedOrChanged = changed flatMap toAddedOrChanged
        for (_ <- addedOrChanged.checkUniqueness(_.path)) yield {
          lazy val addedSet = addedOrChanged.view.collect { case a: ItemAdded => a.path }.toSet
          val deletedEvents = deleted.view
            .filterNot(addedSet)  // delete and change?
            .filter(exists)  // delete unknown?
            .map(ItemDeleted.apply)
          (new View.Single(VersionAdded(versionId)) ++ deletedEvents ++ addedOrChanged)
            .toVector
        }
      }

  private def diffCurrentVersion(base: Repo): Seq[RepoChange] = {
    // Optimized for small differences (less materialization, more views)
    val added = currentItems.view
      .filter(o => !base.exists(o.path))
      .map(o => o.path -> o)
      .toMap[TypedPath, InventoryItem]
    val updated = currentItems.view
      .filter(o => base.pathToItem(o.path).exists(_.id.versionId != o.id.versionId))
    val deleted = base.currentItems.view
      .map(_.path)
      .filter(path => !exists(path) && !added.contains(path))
    (deleted.map(RepoChange.Deleted.apply) ++
      updated.map(RepoChange.Updated.apply) ++
      added.values.view.map(RepoChange.Added.apply)
    ).toVector
      .sortBy(_.path)
  }

  private def checkVersion(versionId: VersionId, signedItems: Iterable[Signed[InventoryItem]]): Checked[Vector[Signed[InventoryItem]]] =
    signedItems.toVector.traverse(o =>
      o.value.id.versionId match {
        case `versionId` => Right(o)
        case _ => Left(ItemVersionDoesNotMatchProblem(versionId, o.value.id))
      })

  private def toAddedOrChanged(signedItem: Signed[InventoryItem]): Option[RepoEvent.ItemEvent] = {
    val path = signedItem.value.path
    pathToSigned(path) match {
      case Right(`signedItem`) => None
      case Right(_) => Some(ItemChanged(signedItem))
      case Left(_) => Some(ItemAdded(signedItem))
    }
  }

  def typedCount[A <: InventoryItem](implicit A: InventoryItem.Companion[A]): Int =
    pathToVersionToSignedItems.values.view.flatten.count {
      case Entry(_, Some(signed)) if signed.value.companion == A => true
      case _ => false
    }

  def currentTyped[A <: InventoryItem](implicit A: InventoryItem.Companion[A]): Map[A#Path, A] =
    typeToPathToCurrentItem(A).view.mapValues(_.value).toMap.asInstanceOf[Map[A#Path, A]]

  def currentItems: View[InventoryItem] =
    currentSignedItems.view.map(_.value)

  def currentSignedItems: View[Signed[InventoryItem]] =
    for {
      versionToItem <- pathToVersionToSignedItems.values.view
      item <- versionToItem.head.maybeSignedItems
    } yield item

  def applyEvents(events: Seq[RepoEvent]): Checked[Repo] = {
    var result = Checked(this)
    val iterator = events.iterator
    while (result.isRight && iterator.hasNext) {
      result = result flatMap (_.applyEvent(iterator.next()))
    }
    result
  }

  def applyEvent(event: RepoEvent): Checked[Repo] =
    event match {
      case VersionAdded(version) =>
        if (versionSet contains version)
          DuplicateKey("VersionId", version)
        else
          Right(copy(
            versions = version :: versions,
            versionSet = versionSet + version))

      case event: ItemEvent =>
        if (versions.isEmpty)
          Problem.pure(s"Missing initial VersionAdded event for Repo")
        else
          event match {
            case event: ItemAdded =>
              if (exists(event.path))
                Left(DuplicateKey("TypedPath", event.path))
              else
                addOrChange(event)

            case event: ItemChanged =>
              pathToItem(event.path)
                .flatMap(_ => addOrChange(event))

            case ItemDeleted(path) =>
              for (_ <- pathToItem(event.path)) yield
                addEntry(path, None)
          }
    }

  private def addOrChange(event: ItemAddedOrChanged): Checked[Repo] = {
    itemVerifier match {
      case Some(verifier) =>
        verifier.verify(event.signedString).map(_.item).flatMap(item =>
          if (event.path != item.path)
            Problem.pure(s"Error in ItemAddedOrChanged event: path=${event.path} does not equal path=${item.path}")
          else if (item.path.isAnonymous)
            Problem.pure(s"Adding an anonymous ${item.companion.name} is not allowed")
          else if (item.id.versionId != versionId)
            EventVersionDoesNotMatchProblem(versionId, event)
          else
            Right(addEntry(item.path, Some(js7.base.crypt.Signed(item withVersion versionId, event.signedString)))))

      case None =>
        Right(addEntry(event.path, Some(event.signed)))
    }
  }

  def verify[A <: InventoryItem](signed: Signed[A]): Checked[A] =
    itemVerifier match {
      case Some(verifier) =>
        verifier.verify(signed.signedString).map(_.item).flatMap(item =>
          if (item != signed.value)
            Left(Problem.pure("item != signed.value"))
          else
            Right(signed.value))

      case None =>
        Right(signed.value)
    }

  private def addEntry(path: TypedPath, itemOption: Option[Signed[InventoryItem]]): Repo = {
    val version = versions.head
    copy(pathToVersionToSignedItems =
      pathToVersionToSignedItems +
        (path ->
          (Entry(version, itemOption) :: pathToVersionToSignedItems.getOrElse(path, Nil))))
  }

  def exists(path: TypedPath): Boolean = pathToVersionToSignedItems
      .get(path) match {
      case None => false
      case Some(entries) => entries.head.maybeSignedItems.isDefined  // Deleted?
    }

  /** Returns the current InventoryItem to a Path. */
  def pathTo[A <: InventoryItem](path: A#Path)(implicit A: InventoryItem.Companion[A]): Checked[A] =
    pathToVersionToSignedItems
      .checked(path)
      .flatMap { entries =>
        val entry = entries.head
        entry
          .maybeSignedItems
          .toChecked(ItemDeletedProblem(path ~ entry.versionId))
          .map(_.value.asInstanceOf[A])
      }

  def pathToItem(path: TypedPath): Checked[InventoryItem] =
    pathToSigned(path)
      .map(_.value)

  private def pathToSigned(path: TypedPath): Checked[Signed[InventoryItem]] =
    pathToVersionToSignedItems
      .checked(path)
      .flatMap { entries =>
        val entry = entries.head
        entry
          .maybeSignedItems
          .toChecked(ItemDeletedProblem(path ~ entry.versionId))
      }

  /** Returns the InventoryItem to a ItemId. */
  def idTo[A <: InventoryItem](id: ItemId[A#Path])(implicit A: InventoryItem.Companion[A]): Checked[A] =
    idToSigned[A](id).map(_.value)

  /** Returns the InventoryItem to a ItemId. */
  def idToSigned[A <: InventoryItem](id: ItemId[A#Path])(implicit A: InventoryItem.Companion[A]): Checked[Signed[A]] =
    for {
      versionToSignedItem <- pathToVersionToSignedItems.checked(id.path)
      itemOption <- versionToSignedItem
        .collectFirst { case Entry(id.versionId, o) => o }
        .toChecked(UnknownKeyProblem("ItemId", id))
        .orElse(
          for {
            history <- historyBefore(id.versionId)
            fb <- findInHistory(versionToSignedItem, history.contains) toChecked UnknownKeyProblem("ItemId", id)
          } yield fb
        ): Checked[Option[Signed[InventoryItem]]]
      signedItem <- itemOption.toChecked(ItemDeletedProblem(id))
    } yield signedItem.copy(signedItem.value.cast[A])

  lazy val estimatedEventCount: Int = {
    var sum = 1  // VersionAdded
    for (o <- pathToVersionToSignedItems.values) sum += o.size
    sum
  }

  /** Converts the Repo to an event sequence, regarding only a given type. */
  def eventsFor(is: TypedPath.AnyCompanion => Boolean): Seq[RepoEvent] =
    toEvents collect {
      case e: VersionAdded => e
      case e: ItemEvent if is(e.path.companion) => e
    }

  /** Convert the Repo to an event sequence ordered by VersionId. */
  private[item] def toEvents: Seq[RepoEvent] = {
    type DeletedOrUpdated = Either[TypedPath/*deleted*/, Signed[InventoryItem/*added/updated*/]]
    val versionToChanges: Map[VersionId, Seq[DeletedOrUpdated]] =
      pathToVersionToSignedItems.toVector
        .flatMap { case (path, entries) =>
          entries.map(entry =>
            entry.versionId -> entry.maybeSignedItems.fold[DeletedOrUpdated](Left(path))(Right.apply))
        }
        .groupBy(_._1).view.mapValues(_.map(_._2))
        .toMap
    val versionSet = mutable.HashSet() ++ versions
    versions.view.map { version =>
      versionSet -= version
      Vector(VersionAdded(version)) ++
        versionToChanges.getOrElse(version, Nil).map {
          case Left(path) =>
            ItemDeleted(path)
          case Right(signedItem) =>
            val entries = pathToVersionToSignedItems(signedItem.value.path)
            if (findInHistory(entries, versionSet).flatten.isDefined)
              ItemChanged(signedItem)
            else
              ItemAdded(signedItem)
        }
      }
      .toVector.reverse.flatten
  }

  private[item] def historyBefore(versionId: VersionId): Checked[List[VersionId]] =
    versions.dropWhile(versionId.!=) match {
      case Nil => UnknownKeyProblem("VersionId", versionId)
      case _ :: tail => Right(tail)
    }

  def newVersionId(): VersionId =
    VersionId.generate(isKnown = versions.contains)

  override def toString = s"Repo($versions," +  //FIXME
    pathToVersionToSignedItems
      .keys.toSeq.sorted
      .map(path =>
        pathToVersionToSignedItems(path)
          .map(entry => entry.maybeSignedItems.fold(s"${entry.versionId} deleted")(_ => s"${entry.versionId} added"))
      ) + ")"
}

object Repo
{
  val empty = new Repo(Nil, Set.empty, Map.empty, None)

  def signatureVerifying(itemVerifier: InventoryItemVerifier[InventoryItem]): Repo =
    new Repo(Nil, Set.empty, Map.empty, Some(itemVerifier))

  @TestOnly
  private[item] object testOnly {
    implicit final class OpRepo(private val underlying: Repo.type) extends AnyVal {
      def fromOp(versionIds: Seq[VersionId], operations: Iterable[Operation], itemVerifier: Option[InventoryItemVerifier[InventoryItem]]) =
        new Repo(
          versionIds.toList,
          versionIds.toSet,
          operations.toVector.view.reverse.map(_.fold(o => (o.value.id.path, o.value.id.versionId -> Some(o)), o => (o.path, o.versionId -> None)))
            .groupMap(_._1)(_._2)
            .mapValuesStrict(_.map(o => Entry(o._1, o._2)).toList),
          itemVerifier)
    }

    sealed trait Operation {
      def fold[A](whenChanged: Signed[InventoryItem] => A, whenDeleted: ItemId_ => A): A
    }
    final case class Changed(item: Signed[InventoryItem]) extends Operation {
      def fold[A](whenChanged: Signed[InventoryItem] => A, whenDeleted: ItemId_ => A) = whenChanged(item)
    }
    final case class Deleted(id: ItemId_) extends Operation {
      def fold[A](whenChanged: Signed[InventoryItem] => A, whenDeleted: ItemId_ => A) = whenDeleted(id)
    }
  }

  final case class Entry private(versionId: VersionId, maybeSignedItems: Option[Signed[InventoryItem]])

  /** None: not known at or before this VersionId; Some(None): deleted at or before this VersionId. */
  private def findInHistory(entries: List[Entry], isKnownVersion: VersionId => Boolean): Option[Option[Signed[InventoryItem]]] =
    entries
      .dropWhile(e => !isKnownVersion(e.versionId))
      .map(_.maybeSignedItems)
      .headOption
}
