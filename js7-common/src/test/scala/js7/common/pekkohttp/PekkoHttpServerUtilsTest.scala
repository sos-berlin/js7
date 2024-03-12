package js7.common.pekkohttp

import js7.base.test.OurTestSuite
import js7.common.pekkohttp.PekkoHttpServerUtils.*
import org.apache.pekko.http.scaladsl.model.Uri
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
final class PekkoHttpServerUtilsTest extends OurTestSuite, ScalatestRouteTest {

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

  "pathSegment" in {
    def route =
      pathSegment("prefix") {
        complete("A")
      } ~
      pathSegment("prefix/b") {
        complete("B")
      }

    Get("/prefix/") ~> route ~> check {
      assert(responseAs[String] == "A")
    }

    Get("/prefix/x") ~> route ~> check {
      assert(responseAs[String] == "A")
    }

    Get("/prefix/b") ~> route ~> check {
      assert(responseAs[String] == "A")
    }

    Get("/prefix%2Fb/") ~> route ~> check {
      assert(responseAs[String] == "B")
    }
  }

  "pathSegments" in {
    def aRoute =
      pathSegments("prefix") {
        extractUnmatchedPath { path =>
          complete(path.toString)
        }
      }

    def bRoute =
      pathSegments("prefix/b") {
        extractUnmatchedPath { path =>
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
