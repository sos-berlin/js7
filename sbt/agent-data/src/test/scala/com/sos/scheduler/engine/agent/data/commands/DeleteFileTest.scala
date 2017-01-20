package com.sos.scheduler.engine.agent.data.commands

import org.scalatest.FreeSpec
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class DeleteFileTest extends FreeSpec {

  "JSON" in {
    val obj = DeleteFile("FILE")
    val json = """{
      "$TYPE": "DeleteFile",
      "path": "FILE"
    }""".parseJson
    assert((obj: Command).toJson == json)   // Command serializer includes $TYPE
    assert(obj == json.convertTo[Command])
  }
}
