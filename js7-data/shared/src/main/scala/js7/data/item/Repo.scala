package js7.data.item

import cats.instances.either._
import cats.instances.vector._
import cats.syntax.traverse._
import js7.base.crypt.{SignatureVerifier, Signed}
import js7.base.problem.Checked._
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.implicits._
import js7.base.utils.Memoizer
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.{EventVersionDoesNotMatchProblem, ItemVersionDoesNotMatchProblem, VersionedItemDeletedProblem}
import js7.data.agent.AgentId
import js7.data.event.NoKeyEvent
import js7.data.item.BasicItemEvent.ItemAttachedStateChanged
import js7.data.item.ItemAttachedState.{Detached, NotDetached}
import js7.data.item.Repo.Entry
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded, VersionedItemAddedOrChanged, VersionedItemChanged, VersionedItemDeleted, VersionedItemEvent}
import org.jetbrains.annotations.TestOnly
import scala.collection.View

/**
  * Representation of versioned VersionedItem (configuration objects).
  * @param versionSet `versionSet == versions.toSet`
  * @author Joacim Zschimmer
  */
final case class Repo private(
  versions: List[VersionId],
  versionSet: Set[VersionId],
  pathToVersionToSignedItems: Map[ItemPath, List[Entry]],
  idToAgentIdToAttachedState: Map[VersionedItemId_, Map[AgentId, ItemAttachedState.NotDetached]],
  signatureVerifier: Option[SignatureVerifier])
{
  assertThat(versions.nonEmpty || pathToVersionToSignedItems.isEmpty)

  /** `signatureVerifier` is not compared - for testing only. */
  override def equals(o: Any) = o match {
    case o: Repo =>
      versions == o.versions &&
        pathToVersionToSignedItems == o.pathToVersionToSignedItems &&
        idToAgentIdToAttachedState == o.idToAgentIdToAttachedState
    case _ => false
  }

  import Repo._

  lazy val versionId: VersionId = versions.headOption getOrElse VersionId.Anonymous

  def currentVersionSize =
    currentSignedItems.size

  lazy val currentVersion: Map[ItemPath, Signed[VersionedItem]] =
    currentSignedItems.view.map(o => o.value.path -> o).toMap

  private lazy val typeToPathToCurrentItem: VersionedItem.Companion_ => Map[ItemPath, Signed[VersionedItem]] =
    Memoizer.nonStrict1 { companion: VersionedItem.Companion_ =>
      currentVersion collect {
        case (path, signedItem) if signedItem.value.companion == companion =>
          path -> signedItem
      }
    }

  /** Returns the difference to the repo as events. */
  def itemsToEvents(versionId: VersionId, changed: Iterable[Signed[VersionedItem]], deleted: Iterable[ItemPath] = Nil)
  : Checked[Seq[VersionedEvent]] =
    checkItemVersions(versionId, changed)
      .flatMap { changed =>
        val addedOrChanged = changed flatMap toAddedOrChanged
        for (_ <- addedOrChanged.checkUniqueness(_.path)) yield {
          lazy val lazyAddedSet = addedOrChanged.view.collect { case a: VersionedItemAdded => a.path }.toSet
          val deletedEvents = deleted.view
            .filterNot(path => lazyAddedSet contains path)  // delete and change?
            .filter(exists)  // delete unknown?
            .map(VersionedItemDeleted.apply)
          (View(VersionAdded(versionId)) ++ deletedEvents ++ addedOrChanged).toVector match {
            case Vector(VersionAdded(this.versionId)) if changed.nonEmpty || deleted.nonEmpty =>
              // Ignore same version with empty difference if it is not a versionId-only UpdateRepo
              Nil
            case events =>
              events
          }
        }
      }

  private def diffCurrentVersion(base: Repo): Seq[RepoChange] = {
    // Optimized for small differences (less materialization, more views)
    val added = currentItems.view
      .filter(o => !base.exists(o.path))
      .map(o => o.path -> o)
      .toMap[ItemPath, VersionedItem]
    val updated = currentItems.view
      .filter(o => base.pathToItem(o.path).exists(_.id.versionId != o.id.versionId))
    val deleted = base.currentItems.view
      .map(_.path)
      .filter(path => !exists(path) && !added.contains(path))
    (deleted.map(RepoChange.Deleted.apply) ++
      updated.map(RepoChange.Changed.apply) ++
      added.values.view.map(RepoChange.Added.apply)
    ).toVector
      .sortBy(_.path)
  }

  private def checkItemVersions(versionId: VersionId, signedItems: Iterable[Signed[VersionedItem]]): Checked[Vector[Signed[VersionedItem]]] =
    signedItems.toVector.traverse(o =>
      o.value.id.versionId match {
        case `versionId` => Right(o)
        case _ => Left(ItemVersionDoesNotMatchProblem(versionId, o.value.id))
      })

  private def toAddedOrChanged(signedItem: Signed[VersionedItem]): Option[VersionedEvent.VersionedItemEvent] =
    pathToSigned(signedItem.value.path) match {
      case Right(`signedItem`) => None
      case Right(_) => Some(VersionedItemChanged(signedItem))
      case Left(_) => Some(VersionedItemAdded(signedItem))
    }

  def typedCount[A <: VersionedItem](implicit A: VersionedItem.Companion[A]): Int =
    pathToVersionToSignedItems.values.view.flatten.count {
      case Entry(_, Some(signed)) if signed.value.companion == A => true
      case _ => false
    }

  def currentTyped[A <: VersionedItem](implicit A: VersionedItem.Companion[A]): Map[A#Path, A] =
    typeToPathToCurrentItem(A).view.mapValues(_.value).toMap.asInstanceOf[Map[A#Path, A]]

  def currentItems: View[VersionedItem] =
    currentSignedItems.view.map(_.value)

  def currentSignedItems: View[Signed[VersionedItem]] =
    for {
      versionToItem <- pathToVersionToSignedItems.values.view
      item <- versionToItem.head.maybeSignedItems
    } yield item

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
                Left(DuplicateKey("ItemPath", event.path))
              else
                addOrChange(event)

            case event: VersionedItemChanged =>
              pathToItem(event.path)
                .flatMap(_ => addOrChange(event))

            case VersionedItemDeleted(path) =>
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
            else if (item.id.versionId != versionId)
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

  private def addEntry(path: ItemPath, itemOption: Option[Signed[VersionedItem]]): Repo = {
    val version = versions.head
    copy(pathToVersionToSignedItems =
      pathToVersionToSignedItems +
        (path ->
          (Entry(version, itemOption) :: pathToVersionToSignedItems.getOrElse(path, Nil))))
  }

  def exists(path: ItemPath): Boolean = pathToVersionToSignedItems
      .get(path) match {
      case None => false
      case Some(entries) => entries.head.maybeSignedItems.isDefined  // Deleted?
    }

  /** Returns the current VersionedItem to a Path. */
  def pathTo[A <: VersionedItem](path: A#Path)(implicit A: VersionedItem.Companion[A]): Checked[A] =
    pathToVersionToSignedItems
      .checked(path)
      .flatMap(_.head
        .maybeSignedItems
        .toChecked(VersionedItemDeletedProblem(path))
        .map(_.value.asInstanceOf[A]))

  def pathToItem(path: ItemPath): Checked[VersionedItem] =
    pathToSigned(path)
      .map(_.value)

  private def pathToSigned(path: ItemPath): Checked[Signed[VersionedItem]] =
    pathToVersionToSignedItems
      .checked(path)
      .flatMap(_.head
        .maybeSignedItems
        .toChecked(VersionedItemDeletedProblem(path)))

  /** Returns the VersionedItem to a VersionedItemId. */
  def idTo[A <: VersionedItem](id: VersionedItemId[A#Path])(implicit A: VersionedItem.Companion[A]): Checked[A] =
    idToSigned[A](id).map(_.value)

  /** Returns the VersionedItem to a VersionedItemId. */
  def idToSigned[A <: VersionedItem](id: VersionedItemId[A#Path])(implicit A: VersionedItem.Companion[A]): Checked[Signed[A]] =
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
      signedItem <- itemOption.toChecked(VersionedItemDeletedProblem(id.path))
    } yield signedItem.copy(signedItem.value.cast[A])

  lazy val estimatedEventCount: Int = {
    var sum = versions.size  // VersionAdded
    for (o <- pathToVersionToSignedItems.values) sum += o.size
    sum
  }

  /** Convert the Repo to an event sequence ordered by VersionId. */
  def toEvents: View[NoKeyEvent] =
    toVersionedEvents ++ toBasicItemEvents

  private def toVersionedEvents: View[VersionedEvent] = {
    type DeletedOrUpdated = Either[ItemPath/*deleted*/, Signed[VersionedItem/*added/changed*/]]

    val versionToChanges: Map[VersionId, Seq[DeletedOrUpdated]] =
      pathToVersionToSignedItems
        .view
        .flatMap { case (path, entries) =>
          entries.map(entry =>
            entry.versionId -> entry.maybeSignedItems.fold[DeletedOrUpdated](Left(path))(Right.apply))
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
                VersionedItemDeleted(path)
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

  private def toBasicItemEvents: View[BasicItemEvent] =
    for {
      (id, agentToAttached) <- idToAgentIdToAttachedState.view
      (agentId, attachedState) <- agentToAttached
    } yield ItemAttachedStateChanged(id, agentId, attachedState)

  private[item] def historyBefore(versionId: VersionId): Checked[List[VersionId]] =
    versions.dropWhile(versionId.!=) match {
      case Nil => UnknownKeyProblem("VersionId", versionId)
      case _ :: tail => Right(tail)
    }

  def newVersionId(): VersionId =
    VersionId.generate(isKnown = versions.contains)

  def attachedState(itemId: VersionedItemId_, agentId: AgentId): ItemAttachedState =
    idToAgentIdToAttachedState.get(itemId)
      .flatMap(_.get(agentId))
      .getOrElse(Detached)

  def applyBasicItemEvent(event: BasicItemEvent): Checked[Repo] =
    event match {
      case ItemAttachedStateChanged(id: VersionedItemId_, agentId, attachedState) =>
        attachedState match {
          case attachedState: NotDetached =>
            Right(copy(
              idToAgentIdToAttachedState = idToAgentIdToAttachedState +
                (id -> (idToAgentIdToAttachedState.getOrElse(id, Map.empty) + (agentId -> attachedState)))))

          case Detached =>
            Right(
              idToAgentIdToAttachedState.get(id).fold(this) { agentToAttachedState =>
                val updated = agentToAttachedState - agentId
                copy(
                  idToAgentIdToAttachedState =
                    if (updated.isEmpty)
                      idToAgentIdToAttachedState - id
                    else
                      idToAgentIdToAttachedState + (id -> updated))
              })
        }

      case _ => Left(Problem(s"Inapplicable event for Repo: $event"))
    }

  // TODO Very big toString ?
  override def toString = s"Repo($versions," +
    pathToVersionToSignedItems
      .keys.toSeq.sorted
      .map(path =>
        pathToVersionToSignedItems(path)
          .map(entry => entry.maybeSignedItems.fold(s"${entry.versionId} deleted")(_ => s"${entry.versionId} added"))
      ) + ")"
}

object Repo
{
  val empty = new Repo(Nil, Set.empty, Map.empty, Map.empty, None)

  def signatureVerifying(signatureVerifier: SignatureVerifier): Repo =
    new Repo(Nil, Set.empty, Map.empty, Map.empty, Some(signatureVerifier))

  @TestOnly
  private[item] object testOnly {
    implicit final class OpRepo(private val underlying: Repo.type) extends AnyVal {
      def fromOp(versionIds: Seq[VersionId], operations: Seq[Operation], signatureVerifier: Option[SignatureVerifier]) =
        new Repo(
          versionIds.toList,
          versionIds.toSet,
          operations.view
            .reverse
            .map(_.fold(o => (o.value.id.path, o.value.id.versionId -> Some(o)), o => (o.path, o.versionId -> None)))
            .groupMap(_._1)(_._2)
            .view
            .mapValues(_.map(o => Entry(o._1, o._2)).toList)
            .toMap,
          Map.empty,
          signatureVerifier)
    }

    sealed trait Operation {
      def fold[A](whenChanged: Signed[VersionedItem] => A, whenDeleted: VersionedItemId_ => A): A
    }
    final case class Changed(item: Signed[VersionedItem]) extends Operation {
      def fold[A](whenChanged: Signed[VersionedItem] => A, whenDeleted: VersionedItemId_ => A) = whenChanged(item)
    }
    final case class Deleted(id: VersionedItemId_) extends Operation {
      def fold[A](whenChanged: Signed[VersionedItem] => A, whenDeleted: VersionedItemId_ => A) = whenDeleted(id)
    }
  }

  final case class Entry private(versionId: VersionId, maybeSignedItems: Option[Signed[VersionedItem]])

  /** None: not known at or before this VersionId; Some(None): deleted at or before this VersionId. */
  private def findInHistory(entries: List[Entry], isKnownVersion: VersionId => Boolean): Option[Option[Signed[VersionedItem]]] =
    entries
      .view
      .dropWhile(e => !isKnownVersion(e.versionId))
      .map(_.maybeSignedItems)
      .headOption
}
