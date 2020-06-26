package js7.controller.web

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.testkit.ScalatestRouteTest
import js7.base.time.ScalaTime._
import js7.common.http.AkkaHttpUtils.RichHttpResponse
import js7.common.scalautil.Futures.implicits._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RootRouteTest extends AnyFreeSpec with ScalatestRouteTest with RootRoute {

  "/" in {
    Get("/") ~> root ~> check {
      assert(status == NotFound)
      assert(response.utf8StringFuture.await(99.s) == "Try http://example.com/controller/api\n")  // \n is for shell usage
    }
  }
}
