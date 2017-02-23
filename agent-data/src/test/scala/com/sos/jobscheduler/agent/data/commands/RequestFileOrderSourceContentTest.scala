package com.sos.scheduler.engine.agent.data.commands

import java.time.Duration
import org.scalatest.FreeSpec
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class RequestFileOrderSourceContentTest extends FreeSpec {

  "JSON" in {
    val obj = RequestFileOrderSourceContent(
      directory = "DIRECTORY",
      regex = "REGEX",
      duration = Duration.ofMillis(111222333444555666L),
      knownFiles = Set("KNOWN"))
    val json = """{
      "$TYPE": "RequestFileOrderSourceContent",
      "directory": "DIRECTORY",
      "regex": "REGEX",
      "duration": 111222333444555.666,
      "knownFiles": [ "KNOWN" ]
    }""".parseJson
    assert((obj: Command).toJson == json)   // Command serializer includes $TYPE
    assert(obj == json.convertTo[Command])
  }
}
