package com.sos.jobscheduler.core.event.journal.data

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.semigroup._
import com.sos.jobscheduler.base.circeutils.CirceUtils.{RichJson, deriveCodec}
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.journal.data.JournalHeader._
import com.sos.jobscheduler.data.event.{EventId, JournalId}
import io.circe.Json
import java.nio.file.Path
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * @author Joacim Zschimmer
  */
final case class JournalHeader private[data](
  version: String,
  softwareVersion: String,
  buildId: String,
  journalId: JournalId,
  startedAt: Timestamp,
  totalRunningTime: FiniteDuration,
  eventId: EventId,
  totalEventCount: Long,
  timestamp: Timestamp)
{
  def update(eventId: EventId, totalEventCount: Long, totalRunningTime: FiniteDuration) =
    copy(
      version = Version,
      softwareVersion = BuildInfo.version,
      buildId = BuildInfo.buildId,
      eventId = eventId,
      totalEventCount = totalEventCount,
      totalRunningTime = totalRunningTime,
      timestamp = Timestamp.now)
}

object JournalHeader
{
  private[data] val Version = "0.24"  // TODO Vor der ersten Software-Freigabe zu "1" wechseln
  private val logger = Logger(getClass)

  def forTest(journalId: JournalId, eventId: EventId = EventId.BeforeFirst): JournalHeader =
    new JournalHeader(
      version = Version,
      softwareVersion = BuildInfo.version,
      buildId = BuildInfo.buildId,
      journalId,
      Timestamp.now,
      Duration.Zero,
      eventId = eventId,
      totalEventCount = 0,
      Timestamp.now)

  def initial(journalId: JournalId) =
    new JournalHeader(
      version = Version,
      softwareVersion = BuildInfo.version,
      buildId = BuildInfo.buildId,
      journalId,
      startedAt = Timestamp.now,
      totalRunningTime = Duration.Zero,
      eventId = EventId.BeforeFirst,
      totalEventCount = 0,
      Timestamp.now)

  implicit lazy val jsonCodec = {
    intelliJuseImport(FiniteDurationJsonEncoder)
    implicit val x = Timestamp.StringTimestampJsonEncoder
    TypedJsonCodec[JournalHeader](
      Subtype.named(deriveCodec[JournalHeader], "JobScheduler.Journal"))
  }

  def checkedHeader(json: Json, journalFile: Path, expectedJournalId: Option[JournalId]): Checked[JournalHeader] =
    for {
      header <-
        json.as[JournalHeader].toSimpleChecked.mapProblem(problem =>
          Problem.pure(
            s"Not a valid JobScheduler journal file: $journalFile. Expected a JournalHeader instead of ${json.compactPrint}"
          ) |+| problem)
      header <-
        expectedJournalId match {
          case Some(expected) if expected != header.journalId =>
            Invalid(JournalIdMismatchProblem(journalFile, expectedJournalId = expected, foundJournalId = header.journalId))
          case _ => Valid(header)
        }
      header <-
        if (header.version != Version) Invalid(Problem(
          s"Journal file has version ${header.version} but $Version is expected. Incompatible journal file: $journalFile"))
        else {
          for (o <- expectedJournalId) logger.debug(s"JournalHeader of file '${journalFile.getFileName}' is as expected, journalId=$o")
          json.as[JournalHeader].toSimpleChecked
        }
    } yield header

  final case class JournalIdMismatchProblem(file: Path, expectedJournalId: JournalId, foundJournalId: JournalId) extends Problem.Coded {
    def arguments = Map(
      "file" -> file.getFileName.toString,
      "expectedJournalId" -> expectedJournalId.string,
      "foundJournalId" -> foundJournalId.string)
  }
  object JournalIdMismatchProblem extends Problem.Coded.Companion
}
