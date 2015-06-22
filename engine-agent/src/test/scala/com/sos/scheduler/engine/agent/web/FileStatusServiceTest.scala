package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.scheduler.engine.common.scalautil.Closers.withCloser
import java.nio.file.Files
import java.nio.file.Files.createTempFile
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/json`
import spray.http.StatusCodes.{NotFound, OK}
import spray.http.Uri
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class FileStatusServiceTest extends FreeSpec with ScalatestRouteTest with FileStatusService {

  implicit lazy val actorRefFactory = ActorSystem()

  "fileStatus" in {
    withCloser { implicit closer ⇒
      val file = createTempFile("test-", ".tmp") withCloser Files.delete
      Get(Uri("/jobscheduler/agent/fileStatus").withQuery("file" → file.toString)) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
      }
      Get(Uri("/jobscheduler/agent/fileStatus").withQuery("file" → "--UNKNOWN--")) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == NotFound)
      }
    }
  }
}
