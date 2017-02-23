package com.sos.scheduler.engine.agent.data.commands

import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class LogoutTest extends FreeSpec {

  "JSON" in {
    val obj = Logout
    val json = """{
      "$TYPE": "Logout"
    }""".parseJson
    assert((obj: Command).toJson == json)   // Command serializer includes $TYPE
    assert(obj == json.convertTo[Logout.type])
  }
}
