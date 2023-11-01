package js7.controller.web

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.common.http.PekkoHttpUtils.RichHttpResponse
import monix.execution.Scheduler.Implicits.traced
import org.apache.pekko.http.scaladsl.model.StatusCodes.NotFound
import org.apache.pekko.http.scaladsl.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
final class RootRouteTest extends OurTestSuite with ScalatestRouteTest with RootRoute
{
  coupleScribeWithSlf4j()

  override def testConfig = config"pekko.loglevel = warning"
    .withFallback(super.testConfig)

  "/" in {
    Get("/") ~> root ~> check {
      assert(status == NotFound)
      assert(response.utf8String.await(99.s) == "Try http://example.com/controller/api\n")  // \n is for shell usage
    }
  }
}
