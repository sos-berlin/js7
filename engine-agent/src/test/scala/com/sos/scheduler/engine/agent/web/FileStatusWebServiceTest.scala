package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.web.test.WebServiceTest
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.scheduler.engine.common.scalautil.Closers.withCloser
import com.sos.scheduler.engine.common.sprayutils.SimpleTypeSprayJsonSupport._
import java.nio.file.Files
import java.nio.file.Files.createTempFile
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/json`
import spray.http.Uri
import spray.json.JsBoolean

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
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
