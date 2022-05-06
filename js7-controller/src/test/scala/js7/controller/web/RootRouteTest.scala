package js7.controller.web

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.testkit.ScalatestRouteTest
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.common.http.AkkaHttpUtils.RichHttpResponse
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RootRouteTest extends AnyFreeSpec with ScalatestRouteTest with RootRoute
{
  coupleScribeWithSlf4j()

  "/" in {
    Get("/") ~> root ~> check {
      assert(status == NotFound)
      assert(response.utf8String.await(99.s) == "Try http://example.com/controller/api\n")  // \n is for shell usage
    }
  }
}
