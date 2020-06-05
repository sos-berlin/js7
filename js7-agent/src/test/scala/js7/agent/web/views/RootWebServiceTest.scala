package js7.agent.web.views

import akka.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import akka.http.scaladsl.model.headers.Accept
import io.circe.Json
import js7.agent.data.views.AgentOverview
import js7.agent.web.test.WebServiceTest
import js7.base.circeutils.CirceUtils._
import js7.base.system.SystemInformation
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.http.AkkaHttpUtils.RichHttpResponse
import js7.common.http.CirceJsonSupport._
import js7.common.scalautil.Futures.implicits._
import js7.data.system.JavaInformation
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class RootWebServiceTest extends AnyFreeSpec with WebServiceTest with RootWebService
{
  protected def isShuttingDown = false
  protected def scheduler = Scheduler.global
  protected def agentOverview = Task.pure(AgentOverview(
    startedAt = Timestamp.parse("2015-06-01T12:00:00Z"),
    version = "TEST-VERSION",
    buildId = "BUILD-ID",
    isTerminating = false,
    system = SystemInformation(hostname = "TEST-HOSTNAME"),
    java = JavaInformation(
      version = "x.y.z",
      JavaInformation.Memory(maximum = 3, total = 2, free = 1),
      systemProperties = Map("test" -> "TEST"))))

  private def expectedOverviewJson = json"""{
    "startedAt": 1433160000000,
    "version": "TEST-VERSION",
    "buildId": "BUILD-ID",
    "isTerminating": false,
    "system": {
      "hostname": "TEST-HOSTNAME",
      "mxBeans": {}
    },
    "java": {
      "version": "x.y.z",
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
    pathSegments("agent/api") {
      apiRootRoute
    }

  "overview" - {
    "Accept: application/json returns compact JSON" in {
      Get("/agent/api") ~> Accept(`application/json`) ~> route ~> check {
        assert(responseAs[Json] == expectedOverviewJson)
        assert(!(response.utf8StringFuture.await(99.s) contains " ")) // Compact JSON
      }
    }

    "Accept: text/plain returns pretty YAML" in {
      Get("/agent/api") ~> Accept(`text/plain`) ~> route ~> check {
        assert(response.utf8StringFuture.await(99.s) contains " ") // YAML
      }
    }
  }
}
