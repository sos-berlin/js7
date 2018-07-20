package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.circeutils.CirceUtils.{RichJson, deriveCodec}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.BuildInfo
import io.circe.Json
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final case class JournalHeader(
  version: String,
  softwareVersion: String,
  buildId: String,
  timestamp: String)

object JournalHeader
{
  val Singleton = JournalHeader(
    version = "0.16",   // TODO Vor der ersten Software-Freigabe zu "1" wechseln
    softwareVersion = BuildInfo.version,
    buildId = BuildInfo.buildId,
    Timestamp.now.toIsoString)

  implicit lazy val jsonCodec = TypedJsonCodec[JournalHeader](
    Subtype.named(deriveCodec[JournalHeader], "JobScheduler.Journal"))

  def checkHeader(json: Json, journalFile: Path): Unit = {
    val header = json.as[JournalHeader] match {
      case Left(t) ⇒ throw new RuntimeException(s"Not a valid JobScheduler journal file: $journalFile. Expected header ${json.compactPrint}", t)
      case Right(o) ⇒ o
    }
    val expectedHeader = JournalHeader.Singleton
    if (header.version != expectedHeader.version)
      sys.error(s"Journal has version ${header.version} but ${expectedHeader.version} is expected. Incompatible journal file: $journalFile")
  }
}
