package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.common.BuildInfo
import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class JsonJournalHeaderTest extends FreeSpec {

  "NamedJsonFormat" in {
    check(JsonJournalMeta.Header,
      s"""{
        "TYPE": "JobScheduler.Journal",
        "version": "0.1",
        "softwareVersion": "${BuildInfo.version}"
        }""")
  }

  private def check(a: JsonJournalHeader, json: String): Unit = {
    assert(a.toJson == json.parseJson)
    assert(json.parseJson.convertTo[JsonJournalHeader] == a)
  }
}
