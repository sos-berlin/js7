package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.filebased.FileBasedObstacle._
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class FileBasedObstacleTest extends FreeSpec {

  "BadState" in {
    val o: FileBasedObstacle = BadState(FileBasedState.not_initialized, error = Some("ERROR"))
    val json = """{
      "TYPE": "BadState",
      "state": "not_initialized",
      "error": "ERROR"
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[FileBasedObstacle])
  }

  "Replaced" in {
    val o: FileBasedObstacle = Replaced(Some("ERROR"))
    val json = """{
      "TYPE": "Replaced",
      "error": "ERROR"
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[FileBasedObstacle])
  }

  "Removed" in {
    val o: FileBasedObstacle = Removed
    val json = """{
      "TYPE": "Removed"
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[FileBasedObstacle])
  }

  "MissingRequisites" in {
    val o: FileBasedObstacle = MissingRequisites(Set(
      JobPath("/JOB"),
      ProcessClassPath("/PROCESSCLASS"),
      JobChainPath("/JOBCHAIN") orderKey "1"))
    val json = """{
      "TYPE": "MissingRequisites",
      "paths": [
        "Job:/JOB",
        "ProcessClass:/PROCESSCLASS",
        "Order:/JOBCHAIN,1"
      ]
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[FileBasedObstacle])
  }
}
