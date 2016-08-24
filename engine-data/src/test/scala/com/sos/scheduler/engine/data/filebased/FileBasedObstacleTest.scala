package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.filebased.FileBasedObstacle._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class FileBasedObstacleTest extends FreeSpec {

  "Missing" in {
    val o: FileBasedObstacle = Missing
    val json = """{
      "TYPE": "Missing"
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[FileBasedObstacle])
  }

  "BadState" in {
    val o: FileBasedObstacle = BadState(FileBasedState.not_initialized, message = Some("MESSAGE"))
    val json = """{
      "TYPE": "BadState",
      "state": "not_initialized",
      "message": "MESSAGE"
    }""".parseJson
    assert(o.toJson == json)
    assert(o == json.convertTo[FileBasedObstacle])
  }
}
