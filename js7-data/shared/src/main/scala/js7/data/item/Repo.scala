package js7.data.item

import cats.instances.either._
import cats.syntax.traverse._
import js7.base.crypt.{SignatureVerifier, Signed}
import js7.base.problem.Checked._
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.implicits._
import js7.base.utils.Memoizer
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.{EventVersionDoesNotMatchProblem, ItemVersionDoesNotMatchProblem, UnknownItemPathProblem, VersionedItemRemovedProblem}
import js7.data.item.Repo.Entry
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded, VersionedItemAddedOrChanged, VersionedItemChanged, VersionedItemEvent, VersionedItemRemoved}
import org.jetbrains.annotations.TestOnly
import scala.collection.{View, mutable}

/**
  * Representation of versioned VersionedItem (configuration objects).
  * @param versionSet `versionSet == versions.toSet`
  * @author Joacim Zschimmer
  */
final case class Repo private(
  versions: List[VersionId],
  versionSet: Set[VersionId],
  pathToVersionToSignedItems: Map[VersionedItemPath, List[Entry]],
  signatureVerifier: Option[SignatureVerifier])
{
  assertThat(versions.nonEmpty || pathToVersionToSignedItems.isEmpty)

  /** `signatureVerifier` is not compared - for testing only. */
  override def equals(o: Any) = o match {
    case o: Repo =>
      versions == o.versions &&
        pathToVersionToSignedItems == o.pathToVersionToSignedItems
    case _ => false
  }

  import Repo._

  lazy val versionId: VersionId = versions.headOption getOrElse VersionId.Anonymous

  def currentVersionSize =
    currentSignedItems.size

  lazy val currentVersion: Map[VersionedItemPath, Signed[VersionedItem]] =
    currentSignedItems.view.map(o => o.value.path -> o).toMap

  private lazy val typeToPathToCurrentItem: VersionedItem.Companion_ => Map[VersionedItemPath, Signed[VersionedItem]] =
    Memoizer.nonStrict1 { companion: VersionedItem.Companion_ =>
      currentVersion collect {
        case (path, signedItem) if signedItem.value.companion == companion =>
          path -> signedItem
      }
    }

  /** Returns the difference to the repo as events. */
  def itemsToEvents(versionId: VersionId, changed: Seq[Signed[VersionedItem]], removed: Iterable[VersionedItemPath])
  : Checked[Seq[VersionedEvent]] =
    itemsToEventBlock(versionId, changed, removed).map(_.events)

  /** Returns the difference to the repo as events. */
  private[item] def itemsToEventBlock(
    versionId: VersionId,
    changed: Seq[Signed[VersionedItem]],
    removed: Iterable[VersionedItemPath] = Nil)
  : Checked[EventBlock] =
    checkItemVersions(versionId, changed)
      .flatMap { changed =>
        val addedOrChanged = changed.view.flatMap(toAddedOrChanged).toVector
        for (_ <- addedOrChanged.checkUniqueness(_.path)) yield {
          lazy val addedSet = addedOrChanged.view
            .collect { case a: VersionedItemAdded => a.path }
            .toSet
          val removedEvents = removed.view
            .filterNot(path => addedSet contains path)  // only change and remove
            .filter(exists)  // only known
            .map(VersionedItemRemoved(_))
            .toVector
          if (versionId == this.versionId && addedSet.isEmpty && removedEvents.isEmpty
            && (changed.nonEmpty || removed.nonEmpty))
            // Ignore same version with empty difference if it is not a versionId-only UpdateRepo
            emptyEventBlock
          else
            NonEmptyEventBlock(versionId, removedEvents, addedOrChanged)
        }
      }

  private def diffCurrentVersion(base: Repo): Seq[RepoChange] = {
    // Optimized for small differences (less materialization, more views)
    val added = currentItems.view
      .filter(o => !base.exists(o.path))
      .map(o => o.path -> o)
      .toMap[VersionedItemPath, VersionedItem]
    val updated = currentItems.view
      .filter(o => base.isCurrentItem(o.key))
    val removed = base.currentItems.view
      .map(_.path)
      .filter(path => !exists(path) && !added.contains(path))
    (removed.map(RepoChange.Removed.apply) ++
      updated.map(RepoChange.Changed.apply) ++
      added.values.view.map(RepoChange.Added.apply)
    ).toVector
      .sortBy(_.path)
  }

  private def checkItemVersions(versionId: VersionId, signedItems: Seq[Signed[VersionedItem]])
  : Checked[Seq[Signed[VersionedItem]]] =
    signedItems.traverse(o =>
      o.value.key.versionId match {
        case `versionId` => Right(o)
        case _ => Left(ItemVersionDoesNotMatchProblem(versionId, o.value.key))
      })

  private def toAddedOrChanged(signedItem: Signed[VersionedItem]): Option[VersionedEvent.VersionedItemAddedOrChanged] =
    pathToSigned(signedItem.value.path) match {
      case Right(`signedItem`) => None
      case Right(_) => Some(VersionedItemChanged(signedItem))
      case Left(_) => Some(VersionedItemAdded(signedItem))
    }

  def typedCount[A <: VersionedItem](implicit A: VersionedItem.Companion[A]): Int =
    pathToVersionToSignedItems.values.view.flatten.count {
      case Entry(_, Some(signed)) => signed.value.companion eq A
      case _ => false
    }

  def currentTyped[A <: VersionedItem](implicit A: VersionedItem.Companion[A]): Map[A#Path, A] =
    typeToPathToCurrentItem(A).view.mapValues(_.value).toMap.asInstanceOf[Map[A#Path, A]]

  def currentItems: View[VersionedItem] =
    currentSignedItems.view.map(_.value)

  def currentSignedItems: View[Signed[VersionedItem]] =
    for {
      versionToItem <- pathToVersionToSignedItems.values.view
      item <- versionToItem.head.maybeSignedItem
    } yield item

  def items: View[VersionedItem] =
    for {
      versionToItem <- pathToVersionToSignedItems.values.view
      item <- versionToItem.view.flatMap(_.maybeSignedItem.map(_.value))
    } yield item

  def itemIdsFor[P <: VersionedItemPath](companion: VersionedItemPath.Companion[P])
  : View[VersionedItemId[P]] =
    pathToVersionToSignedItems
      .to(View)
      .collect {
        case (path, entries) if path.companion eq companion =>
          entries.map(e => path.asInstanceOf[P] ~ e.versionId)
      }
      .flatten

  private def entry(itemId: VersionedItemId_): Option[Entry] =
    pathToVersionToSignedItems.get(itemId.path)
      .flatMap(_.find(_.versionId == itemId.versionId))


  // TODO unused (maybe useful for garbage collection?)
  private[item] def unusedItemIdsForType[P <: VersionedItemPath](inUse: Set[VersionedItemId[P]])
    (implicit P: VersionedItemPath.Companion[P])
  : View[VersionedItemId[P]] =
    pathToVersionToSignedItems.to(View)
      .collect {
        case (path, entries) if path.companion eq P =>
          (path.asInstanceOf[P], entries)
      }
      .flatMap { case (path, entries) =>
        def isInUse(id: VersionedItemId_) =
          (id.path.companion eq P) && inUse(id.asInstanceOf[VersionedItemId[P]])
        unusedVersionIds(entries, isInUse).map(path ~ _)
      }

  def unusedItemIdsForPaths(paths: Iterable[VersionedItemPath], inUse: Set[VersionedItemId_])
  : View[VersionedItemId_] =
    for {
      path <- paths.view
      entries <- pathToVersionToSignedItems.get(path).view
      v <- unusedVersionIds(entries, inUse)
    } yield path ~ v

  def deleteItem(id: VersionedItemId_): Checked[Repo] =
    for (entries <- pathToVersionToSignedItems
      .checked(id.path)
      .map(_.filter(_.versionId != id.versionId))) yield
      copy(
        pathToVersionToSignedItems =
          entries match {
            case Nil | Entry(_, None) :: Nil => pathToVersionToSignedItems - id.path
            case entries => pathToVersionToSignedItems + (id.path -> entries)
          })

  def applyEvents(events: Seq[VersionedEvent]): Checked[Repo] = {
    var result = Checked(this)
    val iterator = events.iterator
    while (result.isRight && iterator.hasNext) {
      result = result.flatMap(_.applyEvent(iterator.next()))
    }
    result
  }

  def applyEvent(event: VersionedEvent): Checked[Repo] =
    event match {
      case VersionAdded(version) =>
        if (versionSet contains version)
          DuplicateKey("VersionId", version)
        else
          Right(copy(
            versions = version :: versions,
            versionSet = versionSet + version))

      case event: VersionedItemEvent =>
        if (versions.isEmpty)
          Problem.pure(s"Missing initial VersionAdded event for Repo")
        else
          event match {
            case event: VersionedItemAdded =>
              if (exists(event.path))
                Left(DuplicateKey("VersionedItemPath", event.path))
              else
                addOrChange(event)

            case event: VersionedItemChanged =>
              pathToItem(event.path)
                .flatMap(_ => addOrChange(event))

            case VersionedItemRemoved(path) =>
              for (_ <- pathToItem(event.path)) yield
                addEntry(path, None)
          }
    }

  private def addOrChange(event: VersionedItemAddedOrChanged): Checked[Repo] = {
    signatureVerifier match {
      case Some(verifier) =>
        verifier
          .verify(event.signedString)
          .map(_ => event.signed.value)
          .flatMap(item =>
            if (item.path.isAnonymous)
              Problem.pure(s"Adding an anonymous ${item.companion.typeName} is not allowed")
            else if (item.key.versionId != versionId)
              EventVersionDoesNotMatchProblem(versionId, event)
            else
              Right(addEntry(item.path, Some(js7.base.crypt.Signed(item withVersion versionId, event.signedString)))))

      case None =>
        Right(addEntry(event.path, Some(event.signed)))
    }
  }

  def verify[A <: VersionedItem](signed: Signed[A]): Checked[A] =
    signatureVerifier match {
      case Some(verifier) =>
        verifier.verify(signed.signedString)
          .map(_ => signed.value)

      case None =>
        Right(signed.value)
    }

  private def addEntry(path: VersionedItemPath, itemOption: Option[Signed[VersionedItem]]): Repo = {
    val version = versions.head
    copy(pathToVersionToSignedItems =
      pathToVersionToSignedItems +
        (path ->
          (Entry(version, itemOption) :: pathToVersionToSignedItems.getOrElse(path, Nil))))
  }

  def exists(path: VersionedItemPath): Boolean =
    pathToVersionToSignedItems.get(path) match {
      case None => false
      case Some(entries) => !entries.head.isRemoved
    }

  def markedAsRemoved(path: VersionedItemPath): Boolean =
    pathToVersionToSignedItems.get(path).exists(_.head.isRemoved)

  def isCurrentItem(id: VersionedItemId_): Boolean =
    pathToId(id.path)
      .exists(_.versionId == id.versionId)

  private def isHiddenItem(id: VersionedItemId_): Boolean =
    pathToVersionToSignedItems.get(id.path)
      .fold(false)(entries =>
        entries.head.versionId != id.versionId &&
          entries.tail.exists(_.versionId == id.versionId))

  /** Returns the current VersionedItem to a Path. */
  def pathTo[A <: VersionedItem](path: A#Path)(implicit A: VersionedItem.Companion[A]): Checked[A] =
    pathToVersionToSignedItems
      .rightOr(path, UnknownItemPathProblem(path))
      .flatMap(_.head
        .maybeSignedItem
        .toChecked(VersionedItemRemovedProblem(path))
        .map(_.value.asInstanceOf[A]))

  def pathToId[P <: VersionedItemPath](path: P): Option[VersionedItemId[P]] =
    for {
      entries <- pathToVersionToSignedItems.get(path)
      signed <- entries.head.maybeSignedItem
    } yield signed.value.id.asInstanceOf[VersionedItemId[P]]

  def pathToItem(path: VersionedItemPath): Checked[VersionedItem] =
    pathToSigned(path)
      .map(_.value)

  private def pathToItems(path: VersionedItemPath): View[VersionedItem] =
    pathToVersionToSignedItems
      .getOrElse(path, Nil)
      .view
      .collect {
        case Entry(_, Some(Signed(item, _))) => item
      }

  private def previousVersionOf(id: VersionedItemId_): Option[VersionedItem] = {
    val entries = pathToVersionToSignedItems.getOrElse(id.path, Nil)
    if (entries.sizeIs < 2)
      None
    else entries.dropRight(1).last match {
      case Entry(_, Some(Signed(item, _))) => Some(item)
      case _ => None
    }
  }

  private def pathToSigned(path: VersionedItemPath): Checked[Signed[VersionedItem]] =
    pathToVersionToSignedItems
      .checked(path)
      .flatMap(_.head
        .maybeSignedItem
        .toChecked(VersionedItemRemovedProblem(path)))

  def anyIdToItem(id: VersionedItemId_): Checked[VersionedItem] =
    anyIdToSigned(id).map(_.value)

  /** Returns the VersionedItem to a VersionedItemId. */
  def idTo[A <: VersionedItem](id: VersionedItemId[A#Path])(implicit A: VersionedItem.Companion[A])
  : Checked[A] =
    idToSigned[A](id).map(_.value)

  /** Returns the VersionedItem for a VersionedItemId. */
  def idToSigned[A <: VersionedItem](id: VersionedItemId[A#Path])(implicit A: VersionedItem.Companion[A])
  : Checked[Signed[A]] =
    anyIdToSigned(id).map(signed => signed.copy(signed.value.cast[A]))

  /** Returns the VersionedItem for a VersionedItemId. */
  def anyIdToSigned(id: VersionedItemId_): Checked[Signed[VersionedItem]] =
    for {
      versionToSignedItem <- pathToVersionToSignedItems.checked(id.path)
      itemOption <- versionToSignedItem
        .collectFirst { case Entry(id.versionId, o) => o }
        .toChecked(UnknownKeyProblem("VersionedItemId", id))
        .orElse(
          for {
            history <- historyBefore(id.versionId)
            fb <- findInHistory(versionToSignedItem, history.contains) toChecked UnknownKeyProblem("VersionedItemId", id)
          } yield fb
        ): Checked[Option[Signed[VersionedItem]]]
      signedItem <- itemOption.toChecked(VersionedItemRemovedProblem(id.path))
    } yield signedItem

  lazy val estimatedEventCount: Int = {
    var sum = versions.size  // VersionAdded
    for (o <- pathToVersionToSignedItems.values) sum += o.size
    sum
  }

  /** Convert the Repo to an event sequence ordered by VersionId. */
  def toEvents: View[VersionedEvent] = {
    type RemovedOrUpdated = Either[VersionedItemPath/*removed*/, Signed[VersionedItem/*added/changed*/]]

    val versionToChanges: Map[VersionId, Seq[RemovedOrUpdated]] =
      pathToVersionToSignedItems
        .view
        .flatMap { case (path, entries) =>
          entries.map(entry =>
            entry.versionId -> entry.maybeSignedItem.fold[RemovedOrUpdated](Left(path))(Right.apply))
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2).toVector)
        .toMap

    versions
      .foldLeft((versionSet, List.empty[View[VersionedEvent]])) { case ((versionSet, events), version) =>
        val tailVersionSet = versionSet - version
        val nextEvents = View(VersionAdded(version)) ++
          versionToChanges
            .getOrElse(version, Nil)
            .view
            .map {
              case Left(path) =>
                VersionedItemRemoved(path)
              case Right(signedItem) =>
                val entries = pathToVersionToSignedItems(signedItem.value.path)
                if (findInHistory(entries, tailVersionSet).flatten.isDefined)
                  VersionedItemChanged(signedItem)
                else
                  VersionedItemAdded(signedItem)
            }
        (tailVersionSet, nextEvents :: events)
      }
      ._2
      .view
      .flatten
  }

  private[item] def historyBefore(versionId: VersionId): Checked[List[VersionId]] =
    versions.dropWhile(versionId.!=) match {
      case Nil => UnknownKeyProblem("VersionId", versionId)
      case _ :: tail => Right(tail)
    }

  def newVersionId(): VersionId =
    VersionId.generate(isKnown = versions.contains)

  // TODO Very big toString ?
  override def toString = s"Repo($versions," +
    pathToVersionToSignedItems
      .keys.toSeq.sorted
      .map(path => s"$path: " +
        pathToVersionToSignedItems(path)
          .map(entry => entry.maybeSignedItem.fold(s"${entry.versionId} removed")(_ => s"${entry.versionId} added"))
      ) + ")"

  private[item] sealed trait EventBlock {
    def events: Seq[VersionedEvent]
    def removedEvents: Seq[VersionedItemRemoved]
    def ids: Seq[VersionedItemId_]
    def items: Seq[VersionedItem]
    def isEmpty: Boolean
    final def nonEmpty = !isEmpty
  }

  private[item] val emptyEventBlock: EventBlock = EmptyEventBlock

  private[item] case object EmptyEventBlock extends EventBlock {
    def isEmpty = true
    def events = Nil
    def removedEvents = Nil
    def ids = Nil
    def items = Nil
  }

  sealed case class NonEmptyEventBlock(
    versionId: VersionId,
    removedEvents: Seq[VersionedItemRemoved],
    addedOrChanged: Seq[VersionedItemAddedOrChanged])
  extends EventBlock {
    def isEmpty = false
    lazy val events = (View(VersionAdded(versionId)) ++ removedEvents ++ addedOrChanged).toVector
    lazy val ids = {
      (removedEvents.view.map(event => (pathToId(event.path).get: VersionedItemId_)) ++
        addedOrChanged.view.map(_.path ~ versionId)
      ).toVector
    }
    lazy val items = addedOrChanged.view.map(_.signed.value).toVector
  }
}

object Repo
{
  val empty = new Repo(Nil, Set.empty, Map.empty, None)

  def signatureVerifying(signatureVerifier: SignatureVerifier): Repo =
    new Repo(Nil, Set.empty, Map.empty, Some(signatureVerifier))

  private def unusedVersionIds(entries: List[Entry], inUse: VersionedItemId_ => Boolean)
  : View[VersionId] = {
    val remainingVersionIds: Set[VersionId] = entries.view
      .collect {
        case entry @ Entry(_, Some(Signed(item, _)))
          if inUse(item.id) =>
          entry
        case entry @ Entry(_, None) =>
          entry
      }
      .map(_.versionId)
      .toSet
    val removeVersions = entries.view
      .filterNot(e => remainingVersionIds(e.versionId))
      .map(_.versionId)
      .toSeq
    val intermediateResult = removeVersions.view ++
      redundantRemovedIds(entries.view.filterNot(e => removeVersions.contains(e.versionId)))

    entries.head.maybeSignedItem match {
      case Some(Signed(item, _)) if removeVersions.headOption.contains(item.id.versionId) =>
        // Keep the current item despite it is not currently used
        intermediateResult.tail
      case _ =>
        intermediateResult
    }
  }

  private def redundantRemovedIds(entries: Iterable[Entry]): View[VersionId] = {
    val a = entries.toArray
    val result = new mutable.ArrayBuffer[VersionId](a.size)
    for (i <- a.indices) {
      if (a(i).isRemoved && (i == a.length - 1 || a(i + 1).isRemoved)) {
        result += a(i).versionId
      }
    }
    result.view
  }

  @TestOnly
  private[item] object testOnly {
    implicit final class OpRepo(private val underlying: Repo.type) extends AnyVal {
      def fromOp(versionIds: Seq[VersionId], operations: Seq[Operation], signatureVerifier: Option[SignatureVerifier]) =
        new Repo(
          versionIds.toList,
          versionIds.toSet,
          operations.view
            .reverse
            .map(_.fold(o => (o.value.key.path, o.value.key.versionId -> Some(o)), o => (o.path, o.versionId -> None)))
            .groupMap(_._1)(_._2)
            .view
            .mapValues(_.map(o => Entry(o._1, o._2)).toList)
            .toMap,
          signatureVerifier)
    }

    sealed trait Operation {
      def fold[A](whenChanged: Signed[VersionedItem] => A, whenRemoved: VersionedItemId_ => A): A
    }
    final case class Changed(item: Signed[VersionedItem]) extends Operation {
      def fold[A](whenChanged: Signed[VersionedItem] => A, whenRemoved: VersionedItemId_ => A) = whenChanged(item)
    }
    final case class Removed(id: VersionedItemId_) extends Operation {
      def fold[A](whenChanged: Signed[VersionedItem] => A, whenRemoved: VersionedItemId_ => A) = whenRemoved(id)
    }
  }

  final case class Entry private(versionId: VersionId, maybeSignedItem: Option[Signed[VersionedItem]]) {
    def isRemoved = maybeSignedItem.isEmpty
  }

  /** None: not known at or before this VersionId; Some(None): removed at or before this VersionId. */
  private def findInHistory(entries: List[Entry], isKnownVersion: VersionId => Boolean): Option[Option[Signed[VersionedItem]]] =
    entries
      .view
      .dropWhile(e => !isKnownVersion(e.versionId))
      .map(_.maybeSignedItem)
      .headOption
}
