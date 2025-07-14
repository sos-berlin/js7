package js7.data.event

import cats.effect.IO
import cats.implicits.toShow
import fs2.Stream
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Json}
import java.io.FileWriter
import java.nio.file.Path
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.io.NullWriter
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax.{RichJavaClass, RichString, RichThrowableEither}
import js7.base.utils.Tests.isTest
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.event.JournalEvent.{JournalEventsReleased, SnapshotTaken}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.SnapshotableState.*
import scala.util.control.NonFatal

/** A JournaledState with snapshot, JournalState, but without ClusterState handling. */
trait SnapshotableState[S <: SnapshotableState[S]]
extends JournaledState[S]:
  this: S =>

  def companion: SnapshotableState.Companion[S]

  def name: String

  def toSnapshotStream: Stream[fs2.Pure, Any]

  def estimatedSnapshotSize: Int

  def standards: Standards

  def withStandards(standards: Standards): S

  final def journalState: JournalState =
    standards.journalState

  final def clusterState: ClusterState =
    standards.clusterState

  protected final def applyStandardEvent(keyedEvent: KeyedEvent[Event]): Checked[S] =
    keyedEvent match
      case KeyedEvent(_: NoKey, _: SnapshotTaken) =>
        Right(this)

      case KeyedEvent(_: NoKey, event: JournalEventsReleased) =>
        journalState.applyEvent(event).map: o =>
          withStandards(standards.copy(
            journalState = o))

      case KeyedEvent(NoKey, _: ClusterEvent) =>
        if !isInstanceOf[ClusterableState[?]] then
          Left(Problem(s"ClusterEvent but ${getClass.simpleScalaName} is not a ClusterableState"))
        else
          for o <- clusterState.applyKeyedEvent(keyedEvent.asInstanceOf[KeyedEvent[ClusterEvent]])
            yield withStandards(standards.copy(
              clusterState = o))

      case _ => eventNotApplicable(keyedEvent)

  def eventId: EventId

  /** For testing, should be equal to this. */
  final def toRecovered: S =
    given Codec[Any] = companion.snapshotObjectJsonCodec
    companion.fromStreamSync:
      toSnapshotStream.map(_.asJson.as[Any].orThrow)
    .withEventId(eventId)


object SnapshotableState:
  private val logger = Logger[this.type]

  def logBoth[S <: SnapshotableState[S]](
    a: S, aName: String,
    b: S, bName: String,
    errorFile: Option[Path] = None)
  : Unit =
    autoClosing(
      errorFile match
        case Some(errorFile: Path) if isTest =>
          try
            logger.error(s"Diff is also in file://${errorFile.toAbsolutePath}") // clickable in IntelliJ
            new FileWriter(errorFile.toFile)
          catch case NonFatal(_) => NullWriter()
        case _ => NullWriter()
    ): errorFile =>
      def logLine(line: String) =
        logger.error(line)
        errorFile.write(line)
        errorFile.write('\n')

      logLine(s"$aName = ⏎")
      a.emitLineStream(logLine)

      logLine(s"$bName = ⏎")
      b.emitLineStream(logLine)


  final case class Standards(journalState: JournalState, clusterState: ClusterState):
    def snapshotSize: Int =
      journalState.estimatedSnapshotSize + clusterState.estimatedSnapshotSize

    def toSnapshotStream: Stream[fs2.Pure, Any] =
      journalState.toSnapshotStream ++
        clusterState.toSnapshotStream

    def isEmpty: Boolean =
      this == Standards.empty

    inline def nonEmpty: Boolean =
      !isEmpty


  object Standards:
    val empty: Standards = Standards(JournalState.empty, ClusterState.Empty)


  trait HasSnapshotCodec:
    def snapshotObjectJsonCodec: TypedJsonCodec[Any]


  trait HasCodec
  extends HasSnapshotCodec, JournaledState.HasEventCodec:
    def name: String // Defined in BasicState.Companion


  trait Companion[S <: SnapshotableState[S]]
  extends JournaledState.Companion[S], HasCodec:
    final given snapshotableStateCompanion: Companion[S] = this

    def empty: S

    def newRecoverer(): SnapshotableStateRecoverer[S]

    final def fromStream(snapshotObjects: Stream[IO, Any]): IO[S] =
      IO.defer:
        val recoverer = newRecoverer()
        snapshotObjects.chunks.foreach: snapshots =>
          IO:
            snapshots.foreach: o =>
              recoverer.addSnapshotObject(o)
        .compile.drain
        .productR:
          IO:
            recoverer.result()

    final def fromStreamSync(snapshotObjects: Stream[fs2.Pure, Any]): S =
      val recoverer = newRecoverer()
      snapshotObjects.chunks.map: snapshots =>
        snapshots.foreach: o =>
          recoverer.addSnapshotObject(o)
      .compile.drain
      recoverer.result()

    def updateStaticReference(s: S): Unit =
      ()

    private lazy val journalDecoder: Decoder[Any] =
      val stampedEventDecoder = implicitly[Decoder[Stamped[KeyedEvent[Event]]]]
      stampedEventDecoder or
        snapshotObjectJsonCodec or
        JournalHeader.jsonCodec.asInstanceOf[Decoder[Any]]

    def decodeJournalJson(json: Json): Checked[Any] =
      if !json.isObject then
        Right(json) // JournalSeparator
      else
        journalDecoder.decodeJson(json) match
          case Left(t: io.circe.DecodingFailure) =>
            val problem = Problem.pure(s"Unexpected JSON: ${t.show}")
            logger.error(s"$problem: ${json.compactPrint.truncateWithEllipsis(100)}")
            Left(problem)

          case Right(o) =>
            Right(o)
