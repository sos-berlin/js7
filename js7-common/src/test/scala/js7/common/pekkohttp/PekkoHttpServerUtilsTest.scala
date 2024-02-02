package js7.common.pekkohttp

import cats.effect.IO
import fs2.Stream
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.common.pekkohttp.PekkoHttpServerUtils.*
import js7.common.pekkohttp.PekkoHttpServerUtilsTest.*
import org.apache.pekko.http.scaladsl.model.Uri
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
final class PekkoHttpServerUtilsTest extends OurTestSuite, ScalatestRouteTest {

  override def testConfig = config"pekko.loglevel = warning"
    .withFallback(super.testConfig)

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

  //"completeWithIOStream" - {
  //  "Blue sky case with resource acquisition" in:
  //    val preparation = Preparation()
  //    import preparation.{acquired, released}
  //    val flux = preparation.stream
  //      .evalTap(i => IO:
  //        assert(acquired.get == 1 && released.get == 0)
  //        logger.info(s"--> $i"))
  //      .asFlux
  //    assert(acquired.get == 0 && released.get == 0)
  //
  //    val result = flux.toIterable(1).asScala.toSeq // Blocks !!!
  //    assert(result == Seq(1, 2, 3))
  //    assert(acquired.get == 1 && released.get == 1)
  //
  //  "Failing Stream" in:
  //    val preparation = Preparation()
  //    import preparation.{acquired, last, released}
  //    val flux = preparation.stream
  //      .flatMap(_ +: Stream.raiseError[IO](new TestException))
  //      .asFlux
  //
  //    intercept[TestException]:
  //      flux.toIterable(1).asScala.toSeq
  //    assert(acquired.get == 1 && released.get == 1 && last.get == 1/*not 2 ???*/)
  //
  //  "Canceled Stream" in:
  //    val preparation = new Preparation
  //    import preparation.{acquired, last, released}
  //    val flux = preparation
  //      .stream
  //      .flatMap(_ +: Stream.never[IO])
  //      .asFlux
  //
  //    val disposable = flux.subscribe()
  //    sleep(100.ms)
  //    assert(acquired.get == 1 && released.get == 0 && last.get == 1)
  //    disposable.dispose()
  //    // dispose() cancels in background. So we wait for the outcome:
  //    awaitAndAssert(acquired.get == 1 && released.get == 1 && last.get == 1)
  //
  //
  //  final class TestException extends RuntimeException
  //
  //  final class Preparation:
  //    val acquired, released, last = Atomic(0)
  //
  //    val stream = Stream
  //      .bracket(
  //        acquire = IO:
  //          acquired += 1)(
  //        release = _ => IO:
  //          released += 1)
  //      .flatMap: i =>
  //        assert(i == 1 && acquired.get == 1 && released.get == 0)
  //        Stream(i, 2, 3)
  //      .evalTap(i => IO:
  //        last := i)
  //}
}

object PekkoHttpServerUtilsTest:
  private val logger = Logger[this.type]
