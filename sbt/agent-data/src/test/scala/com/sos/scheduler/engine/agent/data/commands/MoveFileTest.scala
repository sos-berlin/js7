package com.sos.scheduler.engine.agent.data.commands

import org.scalatest.FreeSpec
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class MoveFileTest extends FreeSpec {

   "JSON" in {
     val obj = MoveFile("OLD", "TO-DIRECTORY")
     val json = """{
       "$TYPE": "MoveFile",
       "path": "OLD",
       "toDirectory": "TO-DIRECTORY"
     }""".parseJson
     assert((obj: Command).toJson == json)   // Command serializer includes $TYPE
     assert(obj == json.convertTo[Command])
   }
 }
