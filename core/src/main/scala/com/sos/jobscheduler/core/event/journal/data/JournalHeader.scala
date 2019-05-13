package com.sos.jobscheduler.core.event.journal.data

import com.sos.jobscheduler.base.circeutils.CirceUtils.{RichJson, deriveCodec}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.data.event.EventId
import io.circe.Json
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final case class JournalHeader(
  version: String,
  softwareVersion: String,
  buildId: String,
  eventId: EventId,
  totalEventCount: Long,
  timestamp: String)

object JournalHeader
{
  private[data] val Version = "0.20"  // TODO Vor der ersten Software-Freigabe zu "1" wechseln

  def apply(eventId: EventId, totalEventCount: Long) = new JournalHeader(
    version = Version,
    softwareVersion = BuildInfo.version,
    buildId = BuildInfo.buildId,
    eventId = eventId,
    totalEventCount = totalEventCount,
    Timestamp.now.toIsoString)

  implicit lazy val jsonCodec = TypedJsonCodec[JournalHeader](
    Subtype.named(deriveCodec[JournalHeader], "JobScheduler.Journal"))

  def checkHeader(json: Json, journalFile: Path): JournalHeader = {
    val header = json.as[JournalHeader] match {
      case Left(t) => throw new RuntimeException(s"Not a valid JobScheduler journal file: $journalFile. Expected a JournalHeader instead of ${json.compactPrint}", t)
      case Right(o) => o
    }
    if (header.version != Version)
      sys.error(s"Journal file has version ${header.version} but $Version is expected. Incompatible journal file: $journalFile")
    json.as[JournalHeader].orThrow
  }
}
