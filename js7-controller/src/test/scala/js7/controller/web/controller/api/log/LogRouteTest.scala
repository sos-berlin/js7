package js7.controller.web.controller.api.log

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO}
import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.delete
import java.nio.file.Path
import java.util.concurrent.ArrayBlockingQueue
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.implicits.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryFile
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.common.http.StreamingSupport.*
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegment
import js7.controller.web.controller.api.test.RouteTester
import org.apache.pekko.http.scaladsl.model.MediaTypes.`text/plain`
import org.apache.pekko.http.scaladsl.model.StatusCodes.{NotFound, OK}
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.testkit.RouteTestTimeout
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class LogRouteTest extends OurTestSuite, RouteTester, LogRoute:
  protected def whenShuttingDown = Deferred.unsafe
  protected def currentLogFile = _currentLogFile.nn

  override protected def config = config"js7.web.server.services.log.poll-interval = 1.ms"
    .withFallback(super.config)

  private implicit val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(99.s)

  private var _currentLogFile: Path | Null = null

  private given IORuntime = ioRuntime

  private lazy val route = seal(
    pathSegment("log") {
      logRoute
    })

  "/controller/api/log" in:
    withTemporaryFile("LogRouteTest", ".tmp") { file =>
      _currentLogFile = file

      file := "LOG TEXT"
      Get("/log?snapshot=true") ~> Accept(`text/plain`) ~> route ~> check:
        assert(status == OK && entityAs[String] == "LOG TEXT")

      if false then
        // Endless streaming response
        // Pekko testkit seems only support synchronous, blocking calls. So this blocks and fails:
        Get("/log") ~> Accept(`text/plain`) ~> route ~> check:
          assert(status == OK)
          val queue = new ArrayBlockingQueue[String](100)
          val completed = response.entity.dataBytes
            .asFs2Stream()
            .map(_.decodeString(UTF_8))
            .foreach(string => IO:
              queue.add(string))
            .compile.drain
            .unsafeToFuture()
          assert(queue.poll(9, SECONDS) == "LOG TEXT")
          autoClosing(new OutputStreamWriter(new FileOutputStream(file, true))) { out =>
            for text <- Array("/ZWEI", "/DREI") do
              out.write(text)
              out.flush()
              assert(queue.poll(9, SECONDS) == text)
          }
          delete(file)
          completed.await(99.s)

      Get("log") ~> Accept(`text/plain`) ~> route ~> check:
        assert(status == NotFound && entityAs[String] == "The requested resource could not be found.")
    }
