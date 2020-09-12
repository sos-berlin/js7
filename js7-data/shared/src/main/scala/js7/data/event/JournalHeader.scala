package js7.data.event

import cats.syntax.semigroup._
import io.circe.Json
import java.nio.file.Path
import js7.base.BuildInfo
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.event.JournalHeader._
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * @author Joacim Zschimmer
  */
final case class JournalHeader private[data](
  journalId: JournalId,
  eventId: EventId,
  generation: Long,
  totalEventCount: Long,
  totalRunningTime: FiniteDuration,
  timestamp: Timestamp,
  startedAt: Timestamp,
  version: String,
  softwareVersion: String,
  buildId: String)
{
  def nextGeneration(eventId: EventId, totalEventCount: Long, totalRunningTime: FiniteDuration, timestamp: Timestamp = Timestamp.now) =
    copy(
      eventId = eventId,
      generation = generation + 1,
      totalEventCount = totalEventCount,
      totalRunningTime = totalRunningTime,
      timestamp = timestamp,
      version = Version,
      softwareVersion = BuildInfo.version,
      buildId = BuildInfo.buildId)

  override def toString = s"JournalHeader($journalId, $eventId, #$generation, total=$totalEventCount, " +
    s"$timestamp, ${totalRunningTime.pretty}, $startedAt, $version, $softwareVersion, $buildId)"
}

object JournalHeader
{
  private[data] val Version = "0.30"  // TODO Vor der ersten Software-Freigabe zu "1" wechseln

  def forTest(journalId: JournalId, eventId: EventId = EventId.BeforeFirst): JournalHeader =
    new JournalHeader(
      journalId,
      eventId = eventId,
      generation = 1,
      totalEventCount = 0,
      Duration.Zero,
      timestamp = Timestamp.now,
      startedAt = Timestamp.now,
      softwareVersion = BuildInfo.version,
      version = Version,
      buildId = BuildInfo.buildId)

  def initial(journalId: JournalId) =
    new JournalHeader(
      journalId,
      eventId = EventId.BeforeFirst,
      generation = 0,
      totalEventCount = 0,
      Duration.Zero,
      timestamp = Timestamp.now,
      startedAt = Timestamp.now,
      version = Version,
      softwareVersion = BuildInfo.version,
      buildId = BuildInfo.buildId)

  implicit lazy val jsonCodec = {
    intelliJuseImport(FiniteDurationJsonEncoder)
    implicit val x = Timestamp.StringTimestampJsonEncoder
    TypedJsonCodec[JournalHeader](
      Subtype.named(deriveCodec[JournalHeader], "JS7.Journal"))
  }

  def checkedHeader(json: Json, journalFileForInfo: Path, expectedJournalId: Option[JournalId]): Checked[JournalHeader] =
    for {
      header <-
        json.as[JournalHeader].toChecked.mapProblem(problem =>
          Problem.pure(
            s"Not a valid JS7 journal file: $journalFileForInfo. Expected a JournalHeader instead of ${json.compactPrint}:"
          ) |+| problem)
      _ <- checkedHeader(header, journalFileForInfo, expectedJournalId)
    } yield header

  def checkedHeader(header: JournalHeader, journalFileForInfo: Path, expectedJournalId: Option[JournalId]): Checked[Unit] =
    for (header <-
      expectedJournalId match {
        case Some(expected) if expected != header.journalId =>
          Left(JournalIdMismatchProblem(journalFileForInfo, expectedJournalId = expected, foundJournalId = header.journalId))
        case _ => Right(header)
      })
      yield
        if (header.version != Version) Left(Problem(
          s"Journal file has version ${header.version} but $Version is expected. Incompatible journal file: $journalFileForInfo"))
        else {
          for (o <- expectedJournalId) scribe.debug(s"JournalHeader of file '${journalFileForInfo.getFileName}' is as expected, journalId=$o")
          Right(())
        }

  final case class JournalIdMismatchProblem(file: Path, expectedJournalId: JournalId, foundJournalId: JournalId) extends Problem.Coded {
    def arguments = Map(
      "file" -> file.getFileName.toString,
      "expectedJournalId" -> expectedJournalId.string,
      "foundJournalId" -> foundJournalId.string)
  }
  object JournalIdMismatchProblem extends Problem.Coded.Companion
}
