package js7.data.item

import cats.instances.either.*
import cats.syntax.traverse.*
import js7.base.crypt.{GenericSignature, SignatureVerifier, Signed, SignedString}
import js7.base.problem.Checked.*
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.*
import js7.base.utils.Memoizer
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.{EventVersionDoesNotMatchProblem, ItemVersionDoesNotMatchProblem, UnknownItemPathProblem, VersionedItemRemovedProblem}
import js7.data.item.Repo.*
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded, VersionedItemAddedOrChanged, VersionedItemChanged, VersionedItemEvent, VersionedItemRemoved}
import org.jetbrains.annotations.TestOnly
import scala.collection.mutable.ListBuffer
import scala.collection.{MapView, View, mutable}

/**
  * Representation of versioned VersionedItem (configuration objects).
  * @param versionIdSet `versionIdSet == versionIds.toSet`
  * @author Joacim Zschimmer
  */
final case class Repo(
  versionIds: List[VersionId],
  versionIdSet: Set[VersionId],
  pathToVersionToSignedItems: Map[VersionedItemPath, List[Version]],
  signatureVerifier: Option[SignatureVerifier],
  selfTest: Boolean = false):

  assert(versionIds.nonEmpty || pathToVersionToSignedItems.isEmpty)

  /** `signatureVerifier` is not compared - for testing only. */
  override def equals(o: Any): Boolean = o match
    case o: Repo =>
      versionIds == o.versionIds &&
        pathToVersionToSignedItems == o.pathToVersionToSignedItems
    case _ => false

  import Repo.*

  lazy val currentVersionId: VersionId = versionIds.headOption getOrElse VersionId.Anonymous

  def currentVersionSize: Int =
    currentSignedItems.size

  lazy val currentVersion: Map[VersionedItemPath, Signed[VersionedItem]] =
    currentSignedItems.view.map(o => o.value.path -> o).toMap

  private lazy val typeToPathToCurrentItem: VersionedItem.Companion_ => Map[VersionedItemPath, Signed[VersionedItem]] =
    Memoizer.nonStrict1 { (companion: VersionedItem.Companion_) =>
      currentVersion collect:
        case (path, signedItem) if signedItem.value.companion == companion =>
          path -> signedItem
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
        for _ <- addedOrChanged.checkUniqueness(_.path) yield
          lazy val addedSet = addedOrChanged.view
            .collect { case a: VersionedItemAdded => a.path }
            .toSet
          val removedEvents = removed.view
            .filterNot(path => addedSet contains path)  // only change and remove
            .filter(exists)  // only known
            .map(VersionedItemRemoved(_))
            .toVector
          if versionId == this.currentVersionId && addedSet.isEmpty && removedEvents.isEmpty
            && (changed.nonEmpty || removed.nonEmpty) then
            // Ignore same version with empty difference if it is not a versionId-only UpdateRepo
            emptyEventBlock
          else
            NonEmptyEventBlock(versionId, removedEvents, addedOrChanged)
      }

  private def checkItemVersions(versionId: VersionId, signedItems: Seq[Signed[VersionedItem]])
  : Checked[Seq[Signed[VersionedItem]]] =
    signedItems.traverse(o =>
      o.value.key.versionId match {
        case `versionId` => Right(o)
        case _ => Left(ItemVersionDoesNotMatchProblem(versionId, o.value.key))
      })

  private def toAddedOrChanged(signedItem: Signed[VersionedItem]): Option[VersionedEvent.VersionedItemAddedOrChanged] =
    pathToSigned(signedItem.value.path) match
      case Right(`signedItem`) => None
      case Right(_) => Some(VersionedItemChanged(signedItem))
      case Left(_) => Some(VersionedItemAdded(signedItem))

  def typedCount[A <: VersionedItem](implicit A: VersionedItem.Companion[A]): Int =
    pathToVersionToSignedItems.values.view.flatten.count:
      case Add(signed) => signed.value.companion eq A
      case _ => false

  def currentTyped[A <: VersionedItem](implicit A: VersionedItem.Companion[A]): Map[A.Path, A] =
    typeToPathToCurrentItem(A).view.mapValues(_.value).toMap.asInstanceOf[Map[A.Path, A]]

  def currentItems: View[VersionedItem] =
    currentSignedItems.view.map(_.value)

  def currentSignedItems: View[Signed[VersionedItem]] =
    for
      versionToItem <- pathToVersionToSignedItems.values.view
      item <- versionToItem.head.maybeSignedItem
    yield item

  def items: View[VersionedItem] =
    for
      versionToItem <- pathToVersionToSignedItems.values.view
      item <- versionToItem.view.flatMap(_.maybeSignedItem.map(_.value))
    yield item

  def itemIdsFor[P <: VersionedItemPath](companion: VersionedItemPath.Companion[P])
  : View[VersionedItemId[P]] =
    pathToVersionToSignedItems
      .to(View)
      .collect:
        case (path, versions) if path.companion eq companion =>
          versions.map(e => path.asInstanceOf[P] ~ e.versionId)
      .flatten

  // TODO unused (maybe useful for garbage collection?)
  private[item] def unusedItemIdsForType[P <: VersionedItemPath](inUse: Set[VersionedItemId[P]])
    (implicit P: VersionedItemPath.Companion[P])
  : View[VersionedItemId[P]] =
    pathToVersionToSignedItems.to(View)
      .collect:
        case (path, versions) if path.companion eq P =>
          path.asInstanceOf[P] -> versions
      .flatMap { case (path, entries) =>
        def isInUse(id: VersionedItemId_) =
          (id.path.companion eq P) && inUse(id.asInstanceOf[VersionedItemId[P]])
        unusedVersionIds(entries, isInUse).map(path ~ _)
      }

  private def unusedItemIdsForPaths(paths: Iterable[VersionedItemPath], inUse: Set[VersionedItemId_])
  : View[VersionedItemId_] =
    for
      path <- paths.view
      entries <- pathToVersionToSignedItems.get(path).view
      v <- unusedVersionIds(entries, inUse)
    yield path ~ v

  def deleteItem(id: VersionedItemId_): Checked[Repo] =
    for entries <- pathToVersionToSignedItems.checked(id.path) yield
      val updatedEntries = deleteVersionFromEntries(id.versionId, entries)
      if updatedEntries eq entries then
        this
      else
        copy(
          pathToVersionToSignedItems =
            if updatedEntries.isEmpty then
              pathToVersionToSignedItems - id.path
            else
              pathToVersionToSignedItems + (id.path -> updatedEntries),
        ).deleteEmptyVersions(entries
          .view.map(_.versionId)
          .filterNot(updatedEntries.view.map(_.versionId).toSet))

  private def deleteEmptyVersions(versionIdCandidates: View[VersionId]): Repo =
    val delete = versionIdCandidates.filterNot(usedVersions).toSet
    if delete.nonEmpty &&
      pathToVersionToSignedItems.forall(_._2.forall(version => !delete(version.versionId))) then
      copy(
        versionIds = versionIds.filterNot(delete),
        versionIdSet = versionIdSet -- delete)
    else
      this

  private def usedVersions: Set[VersionId] =
    pathToVersionToSignedItems.values.flatMap(_.map(_.versionId)).toSet

  def applyEvents(events: Iterable[VersionedEvent]): Checked[Repo] =
    var result = Checked(this)
    val iterator = events.iterator
    while result.isRight && iterator.hasNext do
      result = result.flatMap(_.applyEvent(iterator.next()))
    result

  def applyEvent(event: VersionedEvent): Checked[Repo] =
    event match
      case VersionAdded(version) =>
        if versionIdSet contains version then
          DuplicateKey("VersionId", version)
        else
          Right(copy(
            versionIds = version :: versionIds,
            versionIdSet = versionIdSet + version))

      case event: VersionedItemEvent =>
        if versionIds.isEmpty then
          Problem.pure("Missing initial VersionAdded event for Repo")
        else
          event match
            case event: VersionedItemAdded =>
              if exists(event.path) then
                Left(DuplicateKey("VersionedItemPath", event.path))
              else
                addOrChange(event)

            case event: VersionedItemChanged =>
              pathToVersionedItem(event.path)
                .flatMap(_ => addOrChange(event))

            case VersionedItemRemoved(path) =>
              for _ <- pathToVersionedItem(event.path) yield
                addEntry(path, Remove(currentVersionId))

  private def addOrChange(event: VersionedItemAddedOrChanged): Checked[Repo] =
    signatureVerifier match
      case Some(verifier) =>
        verifier
          .verify(event.signedString)
          .map(_ => event.signed.value)
          .flatMap(item =>
            if item.path.isAnonymous then
              Problem.pure(s"Adding an anonymous ${item.companion.typeName} is not allowed")
            else if item.key.versionId != currentVersionId then
              EventVersionDoesNotMatchProblem(currentVersionId, event)
            else
              Right(addEntry(
                item.path,
                Add(Signed(item.withVersion(currentVersionId), event.signedString)))))

      case None =>
        Right(addEntry(event.path, Add(event.signed)))

  def verify[A <: VersionedItem](signed: Signed[A]): Checked[A] =
    signatureVerifier match
      case Some(verifier) =>
        verifier.verify(signed.signedString)
          .map(_ => signed.value)

      case None =>
        Right(signed.value)

  private def addEntry(path: VersionedItemPath, version: Version): Repo =
    copy(pathToVersionToSignedItems =
      pathToVersionToSignedItems +
        (path ->
          (version :: pathToVersionToSignedItems.getOrElse(path, Nil))))

  def exists(path: VersionedItemPath): Boolean =
    pathToVersionToSignedItems.get(path) match
      case None => false
      case Some(versions) => !versions.head.isRemoved

  def markedAsRemoved(path: VersionedItemPath): Boolean =
    pathToVersionToSignedItems.get(path).exists(_.head.isRemoved)

  def isCurrentItem(id: VersionedItemId_): Boolean =
    pathToId(id.path)
      .exists(_.versionId == id.versionId)

  /** Returns the current VersionedItem to a Path. */
  def pathTo[A <: VersionedItem](A: VersionedItem.Companion[A])(path: A.Path): Checked[A] =
    pathToVersionToSignedItems
      .rightOr(path, UnknownItemPathProblem(path))
      .flatMap(_.head
        .maybeSignedItem
        .toChecked(VersionedItemRemovedProblem(path))
        .map(_.value.asInstanceOf[A]))

  def pathToId[P <: VersionedItemPath](path: P): Option[VersionedItemId[P]] =
    for
      versions <- pathToVersionToSignedItems.get(path)
      signed <- versions.head.maybeSignedItem
    yield signed.value.id.asInstanceOf[VersionedItemId[P]]

  def pathToVersionedItem(path: VersionedItemPath): Checked[VersionedItem] =
    pathToSigned(path)
      .map(_.value)

  def pathToItems[I <: VersionedItem](I: VersionedItem.Companion[I])
  : MapView[I.Path, View[I]] =
    new MapView[I.Path, View[I]]:
      def get(path: I.Path): Option[View[I]] =
        pathToVersionToSignedItems
          .get(path)
          .map(_.view.collect {
            case Add(signedItem) => signedItem.value.asInstanceOf[I]
          })

      override def contains(path: I.Path) =
        pathToVersionToSignedItems contains path

      def iterator: Iterator[(I.Path, View[I])] =
        pathToVersionToSignedItems
          .iterator
          .collect { case (path, versions) if path.companion eq I.Path =>
            path.asInstanceOf[I.Path] ->
              versions.view.collect:
                case Add(signedItem) => signedItem.value.asInstanceOf[I]
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
  def idTo[A <: VersionedItem](A: VersionedItem.Companion[A])(id: VersionedItemId[A.Path])
  : Checked[A] =
    idToSigned(A)(id).map(_.value)

  /** Returns the VersionedItem for a VersionedItemId. */
  def idToSigned[A <: VersionedItem](A: VersionedItem.Companion[A])(id: VersionedItemId[A.Path])
  : Checked[Signed[A]] =
    for signed <- anyIdToSigned(id) yield
      signed.copy(signed.value.cast(A))

  /** Returns the VersionedItem for a VersionedItemId. */
  def anyIdToSigned(id: VersionedItemId_): Checked[Signed[VersionedItem]] =
    for
      versionToSignedItem <- pathToVersionToSignedItems.checked(id.path)
      maybeItem <- versionToSignedItem
        .collectFirst { case Version(id.versionId, o) => o }
        .toChecked(UnknownKeyProblem("VersionedItemId", id))
        .orElse(
          for
            history <- historyBefore(id.versionId)
            fb <- findInHistory(versionToSignedItem, history.contains)
              .toChecked(UnknownKeyProblem("VersionedItemId", id))
          yield fb
        ): Checked[Option[Signed[VersionedItem]]]
      signedItem <- maybeItem.toChecked(VersionedItemRemovedProblem(id.path))
    yield signedItem

  def signedItems: View[Signed[VersionedItem]] =
    pathToVersionToSignedItems.values.view
      .flatMap(_.flatMap(_.maybeSignedItem))

  lazy val estimatedEventCount: Int =
    var sum = versionIds.size  // VersionAdded
    for o <- pathToVersionToSignedItems.values do sum += o.size
    sum

  /** Convert the Repo to an event sequence ordered by VersionId. */
  def toEvents: View[VersionedEvent] =
    val events = toEvents0
    if selfTest then
      for problem <- selfTestEvents(events).left do throw new AssertionError(problem.toString)
    events

  private def selfTestEvents(events: View[VersionedEvent]): Checked[Unit] =
    Repo.empty.applyEvents(events).flatMap { applied =>
      if applied == this then
        Checked.unit
      else
        Left(Problem.pure("💥 Repo.toEvents self-test failed, events do not match repo"))
    }

  /** Convert the Repo to an event sequence ordered by VersionId. */
  private def toEvents0: View[VersionedEvent] =
    type RemovedOrUpdated = Either[VersionedItemPath/*removed*/, Signed[VersionedItem/*added/changed*/]]

    val versionToChanges: Map[VersionId, Seq[RemovedOrUpdated]] =
      pathToVersionToSignedItems
        .view
        .flatMap { case (path, versions) =>
          versions.map(version =>
            version.versionId -> version.maybeSignedItem.fold[RemovedOrUpdated](Left(path))(Right.apply))
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2).toVector)
        .toMap

    versionIds
      .foldLeft((versionIdSet, List.empty[View[VersionedEvent]])) { case ((versionSet, events), version) =>
        val tailVersionSet = versionSet - version
        val nextEvents = View(VersionAdded(version)) ++
          versionToChanges
            .getOrElse(version, Nil)
            .view
            .map:
              case Left(path) =>
                VersionedItemRemoved(path)
              case Right(signedItem) =>
                val versions = pathToVersionToSignedItems(signedItem.value.path)
                if findInHistory(versions, tailVersionSet).flatten.isDefined then
                  VersionedItemChanged(signedItem)
                else
                  VersionedItemAdded(signedItem)
        (tailVersionSet, nextEvents :: events)
      }
      ._2
      .view
      .flatten

  private[item] def historyBefore(versionId: VersionId): Checked[List[VersionId]] =
    versionIds.dropWhile(versionId.!=) match
      case Nil => UnknownKeyProblem("VersionId", versionId)
      case _ :: tail => Right(tail)

  // TODO Very big toString ?
  override def toString: String =
    if versionIds.isEmpty && pathToVersionToSignedItems.isEmpty then
      "Repo.empty"
    else
      s"Repo(versions: ${ versionIds.map(_.string).mkString(" ") }" + {
        val pathLines = pathToVersionToSignedItems
          .keys.toSeq.sorted
          .map(path => s"$path " + pathToVersionToSignedItems(path).mkString(" "))
        if pathLines.sizeIs <= 1 then " · " + pathLines.mkString
        else pathLines.map("↩︎\n  " + _).mkString
      } + ")"

  private[item] sealed trait EventBlock:
    def events: Seq[VersionedEvent]
    def removedEvents: Seq[VersionedItemRemoved]
    def ids: Seq[VersionedItemId_]
    def items: Seq[VersionedItem]
    def isEmpty: Boolean
    final def nonEmpty = !isEmpty

  private[item] val emptyEventBlock: EventBlock = EmptyEventBlock

  private[item] case object EmptyEventBlock extends EventBlock:
    def isEmpty = true
    def events = Nil
    def removedEvents = Nil
    def ids = Nil
    def items = Nil

  sealed case class NonEmptyEventBlock(
    versionId: VersionId,
    removedEvents: Seq[VersionedItemRemoved],
    addedOrChanged: Seq[VersionedItemAddedOrChanged])
  extends EventBlock:
    def isEmpty = false
    lazy val events: Seq[VersionedEvent] =
      (View(VersionAdded(versionId)) ++ removedEvents ++ addedOrChanged).toVector

    lazy val ids: Seq[VersionedItemId_] =
      (removedEvents.view.map(event => (pathToId(event.path).get: VersionedItemId_)) ++
        addedOrChanged.view.map(_.path ~ versionId)
      ).toVector

    lazy val items: Seq[VersionedItem] =
      addedOrChanged.view.map(_.signed.value).toVector


object Repo:
  val empty = new Repo(Nil, Set.empty, Map.empty, None)

  def signatureVerifying(signatureVerifier: SignatureVerifier): Repo =
    new Repo(Nil, Set.empty, Map.empty, Some(signatureVerifier))

  private def unusedVersionIds(versions: List[Version], inUse: VersionedItemId_ => Boolean)
  : View[VersionId] =
    val remainingVersionIds: Set[VersionId] = versions.view
      .collect:
        case entry @ Add(Signed(item, _)) if inUse(item.id) =>
          entry
        case entry @ Remove(_) =>
          entry
      .map(_.versionId)
      .toSet
    val removeVersions = versions.view
      .filterNot(e => remainingVersionIds(e.versionId))
      .map(_.versionId)
      .toSeq
    val intermediateResult = removeVersions.view ++
      redundantRemovedIds(versions.view.filterNot(e => removeVersions.contains(e.versionId)))

    versions.head.maybeSignedItem match
      case Some(Signed(item, _)) if removeVersions.headOption.contains(item.id.versionId) =>
        // Keep the current item despite it is not currently used
        intermediateResult.tail
      case _ =>
        intermediateResult

  private def redundantRemovedIds(versions: Iterable[Version]): View[VersionId] =
    val a = versions.toArray
    val result = new mutable.ArrayBuffer[VersionId](a.size)
    for i <- a.indices do
      if a(i).isRemoved && (i == a.length - 1 || a(i + 1).isRemoved) then
        result += a(i).versionId
    result.view

  private[item] def deleteVersionFromEntries(versionId: VersionId, versions: List[Version])
  : List[Version] =
    val reverseVersions = versions.toVector.reverse // Older versions first
    reverseVersions.indexWhere(_.versionId == versionId) match
      case -1 => versions
      case cut =>
        if reverseVersions(cut).isRemoved then
          versions // A version marked as Removed cannot be deleted
        else
          val older = reverseVersions.take(cut)
          val newer = reverseVersions.drop(cut + 1)
          removeDuplicateRemoved((older ++ newer).reverse)

  private[item] def removeDuplicateRemoved(versions: Vector[Version]): List[Version] =
    val result = ListBuffer.empty[Version]
    var inRemove = false
    for version <- versions.dropLastWhile(_.isRemoved) do
      if !version.isRemoved then
        inRemove = false
        result += version
      else if !inRemove then
        result += version
        inRemove = true
    result.toList

  /** @param items Only one Version per VersionedItemPath! */
  @TestOnly
  def fromItems(
    items: Seq[VersionedItem],
    signatureVerifier: Option[SignatureVerifier] = None)
  : Repo =
    val signedString = SignedString("", GenericSignature("", ""))
    fromEntries(
      items.view.map(_.id.versionId),
      items
        .map(item => Add(Signed(item, signedString)))
        .groupBy(_.signedItem.value.path),
      signatureVerifier)

  @TestOnly
  def fromEntries(
    versionIds: Iterable[VersionId],
    pathToEntries: Map[VersionedItemPath, Seq[Version]],
    signatureVerifier: Option[SignatureVerifier] = None)
  : Repo =
    new Repo(
      versionIds.toList,
      versionIds.toSet,
      pathToEntries.view.mapValues(_.toList).toMap,
      signatureVerifier)

  sealed trait Version:
    def versionId: VersionId
    def maybeSignedItem: Option[Signed[VersionedItem]]
    def isRemoved: Boolean
  object Version:
    def apply(versionId: VersionId, maybeSignedItem: Option[Signed[VersionedItem]]): Version =
      maybeSignedItem match
        case None => Remove(versionId)
        case Some(signed) => Add(signed)

    def unapply(version: Version): Some[(VersionId, Option[Signed[VersionedItem]])] =
      Some(version.versionId -> version.maybeSignedItem)
  /** Add or updated. */
  final case class Add(signedItem: Signed[VersionedItem])
  extends Version:
    def versionId: VersionId =
      signedItem.value.id.versionId

    val maybeSignedItem: Option[Signed[VersionedItem]] =
      Some(signedItem)

    def isRemoved = false

    override def toString =
      s"+${signedItem.value.id.versionId.string}"
  final case class Remove(versionId: VersionId)
  extends Version:
    def isRemoved = true
    def maybeSignedItem: None.type = None
    override def toString = s"-${versionId.string}"

  /** None: not known at or before this VersionId; Some(None): removed at or before this VersionId. */
  private def findInHistory(versions: List[Version], isKnownVersion: VersionId => Boolean): Option[Option[Signed[VersionedItem]]] =
    versions
      .view
      .dropWhile(e => !isKnownVersion(e.versionId))
      .map(_.maybeSignedItem)
      .headOption
