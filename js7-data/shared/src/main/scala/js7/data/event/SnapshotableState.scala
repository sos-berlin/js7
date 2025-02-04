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
import js7.base.fs2utils.StreamExtensions.mapParallelBatch
import js7.base.io.NullWriter
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.AutoClosing
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.MultipleLinesBracket.{Square, zipWithBracket}
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

  def toSnapshotStream: Stream[IO, Any]

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
  final def toRecovered: IO[S] =
    given Codec[Any] = companion.snapshotObjectJsonCodec
    companion.fromStream:
      Stream.eval(toSnapshotStream.compile.toVector).flatMap(Stream.iterable) // one big Chunk
        .mapParallelBatch():
          _.asJson.as[Any].orThrow
    .map:
      _.withEventId(eventId)

  final def emitLineStream(emit: String => Unit): Unit =
    toLineStream.map(emit).compile.drain

  final def toLineStream: Stream[fs2.Pure, String] =
    toStringStream.through(fs2.text.lines)
      .zipWithBracket(Square)
      .map: (line, br) =>
        br +: line

  def toStringStream: Stream[fs2.Pure, String] =
    Stream.emit(toString)


object SnapshotableState:
  private val logger = Logger[this.type]

  def showDifference[S <: SnapshotableState[S]](
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
      errorFile.write('\n')

      logLine(s"$bName = ⏎")
      b.emitLineStream(logLine)


  final case class Standards(journalState: JournalState, clusterState: ClusterState):
    def snapshotSize: Int =
      journalState.estimatedSnapshotSize + clusterState.estimatedSnapshotSize

    def toSnapshotStream: Stream[IO, Any] =
      journalState.toSnapshotStream ++
        clusterState.toSnapshotStream

  object Standards:
    val empty: Standards = Standards(JournalState.empty, ClusterState.Empty)


  trait HasSnapshotCodec:
    def snapshotObjectJsonCodec: TypedJsonCodec[Any]


  trait HasCodec
  extends HasSnapshotCodec, JournaledState.HasEventCodec:
    def name: String // Defined in BasicState.Companion


  trait Companion[S <: SnapshotableState[S]]
  extends JournaledState.Companion[S], HasCodec:
    implicit final val implicitSnapshotableStateCompanion: Companion[S] = this

    def empty: S

    def newBuilder(): SnapshotableStateBuilder[S]

    final def fromStream(snapshotObjects: Stream[IO, Any]): IO[S] =
      IO.defer:
        val builder = newBuilder()
        snapshotObjects.chunks.foreach: snapshots =>
          IO:
            snapshots.foreach: o =>
              builder.addSnapshotObject(o)
        .compile.drain
        .productR:
          IO:
            builder.onAllSnapshotObjectsAdded()
            builder.result()

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
