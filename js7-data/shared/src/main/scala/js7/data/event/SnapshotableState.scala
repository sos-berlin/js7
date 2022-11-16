package js7.data.event

import cats.implicits.toShow
import io.circe.{Decoder, Json}
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichString
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.SnapshotableState.*
import monix.eval.Task
import monix.reactive.Observable

/** A JournaledState with snapshot, JournalState and ClusterState.. */
trait SnapshotableState[S <: SnapshotableState[S]]
extends JournaledState[S]
{
  this: S =>

  def companion: SnapshotableState.Companion[S]

  def toSnapshotObservable: Observable[Any]

  def estimatedSnapshotSize: Int

  def standards: Standards

  def withStandards(standards: Standards): S

  final def journalState: JournalState =
    standards.journalState

  final def clusterState: ClusterState =
    standards.clusterState

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

  /** For testing, should be equal to this. */
  final def toRecovered: Task[S] =
    companion
      .fromObservable(toSnapshotObservable)
      .map(_.withEventId(eventId))
}

object SnapshotableState
{
  private val logger = Logger[this.type]

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

  trait CompanionForJournal
  {
    def snapshotObjectJsonCodec: TypedJsonCodec[Any]

    implicit def keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event]
  }

  trait Companion[S <: SnapshotableState[S]] extends JournaledState.Companion[S]
  {
    implicit final val implicitSnapshotableStateCompanion: Companion[S] = this

    def empty: S

    def snapshotObjectJsonCodec: TypedJsonCodec[Any]

    def fromObservable(snapshotObjects: Observable[Any]): Task[S] =
      Task.defer {
        val builder = newBuilder()
        snapshotObjects.foreachL(builder.addSnapshotObject)
          .map { _ =>
            builder.onAllSnapshotsAdded()
            builder.result()
          }
      }

    def newBuilder(): SnapshotableStateBuilder[S]

    private lazy val journalDecoder: Decoder[Any] = {
      val stampedEventDecoder = implicitly[Decoder[Stamped[KeyedEvent[Event]]]]
      stampedEventDecoder or
        snapshotObjectJsonCodec or
        JournalHeader.jsonCodec.asInstanceOf[Decoder[Any]]
    }

    def decodeJournalJson(json: Json): Checked[Any] =
      if (!json.isObject)
        Right(json) // JournalSeparator
      else
        journalDecoder.decodeJson(json) match {
          case Left(t: io.circe.DecodingFailure) =>
            val problem = Problem.pure(s"Unexpected JSON: ${ t.show }")
            logger.error(s"$problem: ${json.compactPrint.truncateWithEllipsis(100)}")
            Left(problem)

          case Right(o) =>
            Right(o)
        }
  }
}
