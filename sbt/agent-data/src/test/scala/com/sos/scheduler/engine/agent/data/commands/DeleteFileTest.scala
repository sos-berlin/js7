package com.sos.scheduler.engine.agent.data.commands

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
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
