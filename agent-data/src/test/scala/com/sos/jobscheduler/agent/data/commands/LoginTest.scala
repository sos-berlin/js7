package com.sos.scheduler.engine.agent.data.commands

import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class LoginTest extends FreeSpec {

  "JSON" in {
    val obj = Login
    val json = """{
      "$TYPE": "Login"
    }""".parseJson
    assert((obj: Command).toJson == json)   // Command serializer includes $TYPE
    assert(obj == json.convertTo[Login.type])
  }
}
