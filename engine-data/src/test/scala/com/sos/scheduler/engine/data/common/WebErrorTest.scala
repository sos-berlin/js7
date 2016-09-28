package com.sos.scheduler.engine.data.common

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class WebErrorTest extends FreeSpec {

  "WebError" in {
    check(WebError.Simple(message = "MESSAGE"), """{
        "message": "MESSAGE"
      }""")
  }

  def check(q: WebError.Simple, json: String) = {
    assert(q.toJson == json.parseJson)
    assert(json.parseJson.convertTo[WebError] == q)
  }
}
