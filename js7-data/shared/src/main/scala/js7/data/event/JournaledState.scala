package js7.data.event

import io.circe.Codec
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import js7.data.event.JournaledState._
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.{BasicItemEvent, InventoryItem, InventoryItemEvent, InventoryItemId, ItemPath, SignableItem, SignableItemId, SignableSimpleItem, SimpleItem, SimpleItemPath, UnsignedSimpleItem, VersionedItem}
import monix.eval.Task
import monix.reactive.Observable

trait JournaledState[S <: JournaledState[S]]
extends EventDrivenState[S, Event]
{
  this: S =>

  def toSnapshotObservable: Observable[Any]

  def estimatedSnapshotSize: Int

  def standards: Standards

  def withStandards(standards: Standards): S

  final def journalState: JournalState =
    standards.journalState

  final def clusterState: ClusterState =
    standards.clusterState

  override def applyStampedEvents(stampedEvents: Iterable[Stamped[KeyedEvent[Event]]]): Checked[S] =
    if (stampedEvents.isEmpty)
      Right(this)
    else
      super.applyStampedEvents(stampedEvents)
        .map(_.withEventId(stampedEvents.last.eventId))

  def applyEvent(keyedEvent: KeyedEvent[Event]): Checked[S]

  def withEventId(eventId: EventId): S

  protected final def applyStandardEvent(keyedEvent: KeyedEvent[Event]): Checked[S] =
    keyedEvent match {
      case KeyedEvent(_: NoKey, _: SnapshotTaken) =>
        Right(this)

      case KeyedEvent(_: NoKey, event: JournalEventsReleased) =>
        Right(withStandards(standards.copy(
          journalState = journalState.applyEvent(event))))

      case KeyedEvent(_: ClusterEvent#Key, _: ClusterEvent) =>
        for (o <- clusterState.applyEvent(keyedEvent.asInstanceOf[KeyedEvent[ClusterEvent]]))
          yield withStandards(standards.copy(
            clusterState = o))

      case _ => eventNotApplicable(keyedEvent)
    }

  def eventId: EventId
}

object JournaledState
{
  final case class Standards(journalState: JournalState, clusterState: ClusterState)
  {
    def snapshotSize =
      journalState.estimatedSnapshotSize + clusterState.estimatedSnapshotSize

    def toSnapshotObservable: Observable[Any] =
      journalState.toSnapshotObservable ++
        clusterState.toSnapshotObservable
  }
  object Standards
  {
    def empty = Standards(JournalState.empty, ClusterState.Empty)
  }

  final case class EventNotApplicableProblem(keyedEvent: KeyedEvent[Event], state: Any) extends Problem.Coded {
    def arguments = Map(
      "event" -> keyedEvent.toString.truncateWithEllipsis(100),
      "state" -> state.toString.truncateWithEllipsis(100))
  }

  trait CompanionForJournal
  {
    def snapshotObjectJsonCodec: TypedJsonCodec[Any]

    implicit def keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event]

    //private val jsonDecoder: Decoder[Any] = {
    //  val stampedEventDecoder = implicitly[Decoder[Stamped[KeyedEvent[Event]]]]
    //  stampedEventDecoder or snapshotObjectJsonCodec or JournalHeader.jsonCodec.asInstanceOf[Decoder[Any]]
    //}
    //
    //def decodeJournalJson(json: Json): Checked[Any] =
    //  if (!json.isObject)
    //    Right(json)  // JournalSeparator
    //  else
    //    jsonDecoder.decodeJson(json) match {
    //      case Left(t: io.circe.DecodingFailure) =>
    //        val problem = Problem.pure(s"Unexpected JSON: ${t.show}")
    //        scribe.error(s"$problem: ${json.compactPrint.truncateWithEllipsis(100)}")
    //        Left(problem)
    //
    //      case Right(o) =>
    //        Right(o)
    //    }
  }

  trait Companion[S <: JournaledState[S]] extends CompanionForJournal
  {
    implicit final def implicitCompanion: Companion[S] = this

    def name: String =
      getClass.simpleScalaName

    def empty: S

    def fromObservable(snapshotObjects: Observable[Any]): Task[S] =
      Task.defer {
        val builder = newBuilder()
        snapshotObjects.foreachL(builder.addSnapshotObject)
          .map { _ =>
            builder.onAllSnapshotsAdded()
            builder.result()
          }
      }

    def snapshotObjectJsonCodec: TypedJsonCodec[Any]

    implicit def keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event]

    def newBuilder(): JournaledStateBuilder[S]

    protected def InventoryItems: Seq[InventoryItem.Companion_]


    final lazy val ItemPaths: Seq[ItemPath.AnyCompanion] =
      InventoryItems.collect { case o: VersionedItem.Companion_ => o.Path }

    final lazy val SimpleItems: Seq[SimpleItem.Companion_] =
      InventoryItems collect { case o: SimpleItem.Companion_ => o }

    final lazy val UnsignedSimpleItems: Seq[UnsignedSimpleItem.Companion_] =
      InventoryItems collect { case o: UnsignedSimpleItem.Companion_ => o }

    final lazy val SignableItems: Seq[SignableItem.Companion_] =
      InventoryItems collect { case o: SignableItem.Companion_ => o }

    final lazy val SignableSimpleItems: Seq[SignableSimpleItem.Companion_] =
      InventoryItems collect { case o: SignableSimpleItem.Companion_ => o }

    final lazy val VersionedItems: Seq[VersionedItem.Companion_] =
      InventoryItems collect { case o: VersionedItem.Companion_ => o }


    implicit final lazy val inventoryItemJsonCodec: TypedJsonCodec[InventoryItem] =
      TypedJsonCodec(InventoryItems.map(_.subtype): _*)

    implicit final lazy val inventoryItemEventJsonCodec = InventoryItemEvent.jsonCodec(this)

    implicit final lazy val inventoryItemIdJsonCodec: Codec[InventoryItemId] =
      InventoryItemId.jsonCodec(InventoryItems.map(_.Id))

    implicit final lazy val signableItemIdJsonCodec: Codec[SignableItemId] =
      SignableItemId.jsonCodec(SignableItems.map(_.Id))

    implicit final lazy val unsignedSimpleItemJsonCodec: TypedJsonCodec[UnsignedSimpleItem] =
      TypedJsonCodec(UnsignedSimpleItems.map(_.subtype): _*)

    implicit final lazy val signableSimpleItemJsonCodec: TypedJsonCodec[SignableSimpleItem] =
      TypedJsonCodec(SignableSimpleItems.map(_.subtype): _*)

    implicit final lazy val simpleItemIdJsonCodec: Codec[SimpleItemPath] =
      SimpleItemPath.jsonCodec(SimpleItems.map(_.Id))

    implicit final lazy val itemPathJsonCodec: Codec[ItemPath] =
      ItemPath.jsonCodec(ItemPaths)

    implicit final lazy val basicItemEventJsonCodec =
      BasicItemEvent.jsonCodec(this)

    implicit final lazy val versionedItemJsonCodec: TypedJsonCodec[VersionedItem] =
      TypedJsonCodec(VersionedItems.map(_.subtype): _*)

    implicit final lazy val signableItemJsonCodec: TypedJsonCodec[SignableItem] =
      TypedJsonCodec(SignableItems.map(_.subtype): _*)

    override def toString = name
  }
}
