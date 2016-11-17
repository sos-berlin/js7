package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.job.JobPath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class FileBasedOverviewTest extends FreeSpec {

  "JSON" in {
    val o = FileBasedOverview(
      JobPath("/TEST"),
      FileBasedState.active)
    val json = """{
        "path": "Job:/TEST",
        "fileBasedState": "active"
      }""".parseJson
    assert(o.toJson == json)
    assert(json.convertTo[FileBasedOverview] == o)
  }
}
