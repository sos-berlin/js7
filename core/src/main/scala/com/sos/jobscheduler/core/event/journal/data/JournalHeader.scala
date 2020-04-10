package com.sos.jobscheduler.core.event.journal.data

import cats.syntax.semigroup._
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
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
  private[data] val Version = "0.26"  // TODO Vor der ersten Software-Freigabe zu "1" wechseln
  private val logger = Logger(getClass)

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
      Subtype.named(deriveCodec[JournalHeader], "JobScheduler.Journal"))
  }

  def checkedHeader(json: Json, journalFileForInfo: Path, expectedJournalId: Option[JournalId]): Checked[JournalHeader] =
    for {
      header <-
        json.as[JournalHeader].toChecked.mapProblem(problem =>
          Problem.pure(
            s"Not a valid JobScheduler journal file: $journalFileForInfo. Expected a JournalHeader instead of ${json.compactPrint}:"
          ) |+| problem)
      header <-
        expectedJournalId match {
          case Some(expected) if expected != header.journalId =>
            Left(JournalIdMismatchProblem(journalFileForInfo, expectedJournalId = expected, foundJournalId = header.journalId))
          case _ => Right(header)
        }
      header <-
        if (header.version != Version) Left(Problem(
          s"Journal file has version ${header.version} but $Version is expected. Incompatible journal file: $journalFileForInfo"))
        else {
          for (o <- expectedJournalId) logger.debug(s"JournalHeader of file '${journalFileForInfo.getFileName}' is as expected, journalId=$o")
          json.as[JournalHeader].toChecked
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
