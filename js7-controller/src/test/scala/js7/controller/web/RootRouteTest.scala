package js7.controller.web

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.testkit.ScalatestRouteTest
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.common.http.AkkaHttpUtils.RichHttpResponse
import monix.execution.Scheduler.Implicits.traced

/**
  * @author Joacim Zschimmer
  */
final class RootRouteTest extends OurTestSuite with ScalatestRouteTest with RootRoute:
  override def testConfig = config"akka.loglevel = warning"
    .withFallback(super.testConfig)

  "/" in:
    Get("/") ~> root ~> check:
      assert(status == NotFound)
      assert(response.utf8String.await(99.s) == "Try http://example.com/controller/api\n")  // \n is for shell usage
