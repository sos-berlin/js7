package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.job.JobPath
import java.nio.file.Paths
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class FileBasedDetailedTest extends FreeSpec {

  "JSON" in {
    val file = Paths.get("/DIR/TEST.job.xml")
    val o =
      FileBasedDetailed(
        FileBasedOverview(
        JobPath("/TEST"),
        FileBasedState.active),
        Some(file),
        Some(Instant.parse("2016-09-07T11:22:33.444Z")),
        Some("<?xml version='1.0'?><job/>"))
    val fileJson = '"' + file.toString.replace("""\""", """\\""") + '"'
    val json = s"""{
        "overview": {
          "path": "Job:/TEST",
          "fileBasedState": "active"
        },
        "file": $fileJson,
        "fileModifiedAt": "2016-09-07T11:22:33.444Z",
        "sourceXml": "<?xml version='1.0'?><job/>"
      }""".parseJson
    assert(o.toJson == json)
    assert(json.convertTo[FileBasedDetailed] == o)
  }
}
