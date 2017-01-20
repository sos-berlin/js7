package com.sos.scheduler.engine.agent.data.commandresponses

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class FileOrderSourceContentTest extends FreeSpec {

  "JSON" in {
    val obj = FileOrderSourceContent(
      files = List(FileOrderSourceContent.Entry("FILE", 111222333444555666L)))
    val json = """{
      "files": [
        {
          "path": "FILE",
          "lastModifiedTime": 111222333444555666
        }
      ]
    }""".parseJson
    assert(obj.toJson == json)
    assert(obj == json.convertTo[FileOrderSourceContent])
  }
}
