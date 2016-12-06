package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.sprayutils.SprayUtils._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.Uri
import spray.routing.Directives._
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SprayUtilsTest extends FreeSpec with ScalatestRouteTest {

  "Path" - {
    import Uri.Path

    "startsWithPath" in {
      assert(!(Path("/aa/b") startsWithPath Path("/a")))
      assert(Path("/aa/b") startsWith Path("/a"))  // startsWith matches this

      assert(Path.Empty startsWithPath Path.Empty)
      assert(!(Path.Empty startsWithPath Path("/a")))
      assert(Path("/a") startsWithPath Path("/a"))
      assert(Path("a/b") startsWithPath Path(""))
      assert(Path("a/b") startsWithPath Path("a"))
      assert(Path("a/b") startsWithPath Path("a/"))
      assert(Path("a/b") startsWithPath Path("a/b"))
      assert(!(Path("/b") startsWithPath Path("/a")))
    }

    "drop" in {
      assert(Path.Empty.length == 0)
      assert(Path.Empty.drop(0) == Path.Empty)
      assert(Path("/aa/b").length == 4)
      assert(Path("/aa/b").drop(0) == Path("/aa/b"))
      assert(Path("/aa/b").drop(1) == Path("aa/b"))
      assert(Path("/aa/b").drop(2) == Path("/b"))
      assert(Path("/aa/b").drop(3) == Path("b"))
      assert(Path("/aa/b").drop(4) == Path.Empty)
    }
  }

  "pathSegments" in {
    def aRoute =
      pathSegments("prefix") {
        unmatchedPath { path ⇒
          complete(path.toString)
        }
      }

    def bRoute =
      pathSegments("prefix/b") {
        unmatchedPath { path ⇒
          complete(path.toString)
        }
      }

//    Get("/pre") ~> aRoute ~> check {
//      assert(!handled)
//    }
//
//    Get("/prefix") ~> aRoute ~> check {
//      assert(responseAs[String] == "")
//    }

    Get("/prefix/") ~> aRoute ~> check {
      assert(responseAs[String] == "/")
    }

    Get("/prefix/x") ~> aRoute ~> check {
      assert(responseAs[String] == "/x")
    }

    Get("/prefix/b") ~> bRoute ~> check {
      assert(responseAs[String] == "")
    }

    Get("/prefix/b/") ~> bRoute ~> check {
      assert(responseAs[String] == "/")
    }

    Get("/prefix/b/c") ~> bRoute ~> check {
      assert(responseAs[String] == "/c")
    }
  }
}
