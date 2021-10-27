package js7.data.event

import cats.syntax.semigroup._
import io.circe.Json
import java.nio.file.Path
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport
import scala.concurrent.duration.FiniteDuration

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
  initiallyStartedAt: Timestamp,
  version: String,
  js7Version: String,
  buildId: String)
{
  override def toString = s"JournalHeader($journalId, $eventId, #$generation, total=$totalEventCount, " +
    s"$timestamp, ${totalRunningTime.pretty} (${totalRunningTime.toSeconds}s), $initiallyStartedAt, " +
    s"$version, $js7Version, $buildId)"
}

object JournalHeader
{
  val Version = "1"
  private val compatibility = Map("0.42" -> "1")

  implicit val jsonCodec = {
    intelliJuseImport(FiniteDurationJsonEncoder)
    implicit val x = Timestamp.StringTimestampJsonEncoder

    TypedJsonCodec[JournalHeader](
      Subtype.named[JournalHeader](
        deriveRenamingCodec[JournalHeader](Map(
          "startedAt" -> "initiallyStartedAt"/*COMPATIBLE with 2.0*/)),
        "JS7.Journal"))
  }

  def checkedHeader(json: Json, journalFileForInfo: Path, expectedJournalId: JournalId): Checked[JournalHeader] =
    for {
      header <-
        json.as[JournalHeader].toChecked.mapProblem(problem =>
          Problem.pure(
            s"Not a valid JS7 journal file: $journalFileForInfo. Expected a JournalHeader instead of ${json.compactPrint}:"
          ) |+| problem)
      _ <- checkedHeader(header, journalFileForInfo, Some(expectedJournalId))
    } yield header

  def checkedHeader(header: JournalHeader, journalFileForInfo: Path, expectedJournalId: Option[JournalId]): Checked[Unit] =
    for (header <-
      expectedJournalId match {
        case Some(expected) if expected != header.journalId =>
          Left(JournalIdMismatchProblem(journalFileForInfo, expectedJournalId = expected, foundJournalId = header.journalId))
        case _ => Right(header)
      })
      yield
        if (compatibility.getOrElse(header.version, header.version) != Version)
          Left(Problem(
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
