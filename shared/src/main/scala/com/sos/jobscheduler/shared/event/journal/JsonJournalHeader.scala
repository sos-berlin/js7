package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class JsonJournalHeader(
  version: String,
  softwareVersion: String)

object JsonJournalHeader {

  private val headerJsonFormat = jsonFormat2(apply)
  implicit val jsonFormat = TypedJsonFormat[JsonJournalHeader](
    Subtype(headerJsonFormat, "JobScheduler.Journal"))

}
