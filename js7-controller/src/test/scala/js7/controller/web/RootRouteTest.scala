package js7.controller.web

import cats.effect.unsafe.IORuntime
import org.apache.pekko.http.scaladsl.model.StatusCodes.NotFound
import org.apache.pekko.http.scaladsl.testkit.ScalatestRouteTest
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.{OurTestSuite}
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.common.http.PekkoHttpUtils.RichHttpResponse

/**
  * @author Joacim Zschimmer
  */
final class RootRouteTest extends OurTestSuite, ScalatestRouteTest, RootRoute:

  private given IORuntime = ioRuntime

  override def testConfig = config"pekko.loglevel = warning"
    .withFallback(super.testConfig)

  "/" in:
    Get("/") ~> root ~> check:
      assert(status == NotFound)
      assert(response.utf8String.await(99.s) == "Try http://example.com/controller/api\n")  // \n is for shell usage
