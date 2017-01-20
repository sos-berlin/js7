package com.sos.scheduler.engine.agent.data.commands

import org.scalatest.FreeSpec
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class AbortImmediatelyTest extends FreeSpec {

  "JSON" in {
    val obj = AbortImmediately
    val json = """{ "$TYPE": "AbortImmediately" }""".parseJson
    assert((obj: Command).toJson == json)   // Command serializer includes $TYPE
    assert(obj == json.convertTo[Command])
  }
}
