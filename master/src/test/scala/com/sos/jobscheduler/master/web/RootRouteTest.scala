package com.sos.jobscheduler.master.web

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.common.http.AkkaHttpUtils.RichHttpResponse
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.base.time.ScalaTime._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RootRouteTest extends AnyFreeSpec with ScalatestRouteTest with RootRoute {

  "/" in {
    Get("/") ~> root ~> check {
      assert(status == NotFound)
      assert(response.utf8StringFuture.await(99.s) == "Try http://example.com/master/api\n")  // \n is for shell usage
    }
  }
}
