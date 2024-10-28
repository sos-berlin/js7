package js7.agent.web.views

import cats.effect
import org.apache.pekko.http.scaladsl.model.MediaTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.headers.Accept
import io.circe.Json
import js7.agent.data.views.AgentOverview
import js7.agent.web.test.WebServiceTest
import js7.base.circeutils.CirceUtils.*
import js7.base.system.SystemInformation
import js7.base.test.{OurTestSuite}
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegments
import js7.common.pekkohttp.CirceJsonSupport.*
import js7.common.http.PekkoHttpUtils.RichHttpResponse
import js7.data.system.JavaInformation
import cats.effect.IO
import cats.effect.Deferred
import cats.effect.unsafe.IORuntime

/**
 * @author Joacim Zschimmer
 */
final class RootWebServiceTest extends OurTestSuite, WebServiceTest, RootWebService:

  private given IORuntime = ioRuntime

  protected def whenShuttingDown = Deferred.unsafe

  protected def agentOverview = IO.pure(AgentOverview(
    startedAt = Timestamp.parse("2015-06-01T12:00:00Z"),
    version = "TEST-VERSION",
    buildId = "BUILD-ID",
    //isTerminating = false,
    system = SystemInformation(hostname = "TEST-HOSTNAME"),
    java = JavaInformation(
      version = "x.y.z",
      availableProcessors = 8,
      JavaInformation.Memory(maximum = 3, total = 2, free = 1),
      systemProperties = Map("test" -> "TEST"))))

  private def expectedOverviewJson = json"""{
    "startedAt": 1433160000000,
    "version": "TEST-VERSION",
    "buildId": "BUILD-ID",
    "system": {
      "hostname": "TEST-HOSTNAME",
      "mxBeans": {}
    },
    "java": {
      "version": "x.y.z",
      "availableProcessors": 8,
      "memory": {
        "maximum": 3,
        "total": 2,
        "free": 1
      },
      "systemProperties": {
        "test": "TEST"
      }
    }
  }"""

  private val route =
    pathSegments("agent/api"):
      apiRootRoute

  "overview" - {
    "Accept: application/json returns compact JSON" in {
      Get("/agent/api") ~> Accept(`application/json`) ~> route ~> check:
        assert(responseAs[Json] == expectedOverviewJson)
        assert(!response.utf8String.await(99.s).contains(" ")) // Compact JSON
    }}
