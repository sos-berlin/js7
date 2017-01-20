package com.sos.scheduler.engine.agent.data.commands

import com.sos.scheduler.engine.common.time.ScalaTime._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TerminateTest extends FreeSpec {

  "JSON without sigkillProcessesAfter" in {
    val obj = Terminate(sigtermProcesses = true)
    val json = """{
      "$TYPE":"Terminate",
      "sigtermProcesses": true
    }""".parseJson
    assert((obj: Command).toJson == json)   // Command serializer includes $TYPE
    assert(obj == json.convertTo[Command])
  }

  "JSON with sigkillProcessesAfter" in {
    val obj = Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(30.s))
    val json = """{
      "$TYPE":"Terminate",
      "sigtermProcesses": true,
      "sigkillProcessesAfter": 30
    }""".parseJson
    //assert((obj: Command).toJson == json)   // Command serializer includes $TYPE
    assert(obj == json.convertTo[Command])
  }
}
