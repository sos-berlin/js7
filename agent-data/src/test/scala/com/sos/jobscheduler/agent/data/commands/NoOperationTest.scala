package com.sos.scheduler.engine.agent.data.commands

import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class NoOperationTest extends FreeSpec {

  "JSON" in {
    val obj = NoOperation
    val json = """{
      "$TYPE": "NoOperation"
    }""".parseJson
    assert((obj: Command).toJson == json)   // Command serializer includes $TYPE
    assert(obj == json.convertTo[NoOperation.type])
  }
}
