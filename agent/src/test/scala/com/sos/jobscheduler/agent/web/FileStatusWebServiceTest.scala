package com.sos.jobscheduler.agent.web

import com.sos.jobscheduler.agent.web.test.WebServiceTest
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.sprayutils.SimpleTypeSprayJsonSupport._
import java.nio.file.Files
import java.nio.file.Files.createTempFile
import org.scalatest.FreeSpec
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/json`
import spray.http.Uri
import spray.json.JsBoolean

/**
 * @author Joacim Zschimmer
 */
final class FileStatusWebServiceTest extends FreeSpec with WebServiceTest with FileStatusWebService {

  "fileExists" in {
    withCloser { implicit closer ⇒
      val file = createTempFile("test-", ".tmp") withCloser Files.delete
      Get(Uri("/jobscheduler/agent/api/fileExists").withQuery("file" → file.toString)) ~> Accept(`application/json`) ~> route ~> check {
        assert(responseAs[JsBoolean].value)
      }
      Get(Uri("/jobscheduler/agent/api/fileExists").withQuery("file" → "--UNKNOWN--")) ~> Accept(`application/json`) ~> route ~> check {
        assert(!responseAs[JsBoolean].value)
      }
    }
  }
}
