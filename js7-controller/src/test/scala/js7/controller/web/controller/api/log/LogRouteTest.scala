package js7.controller.web.controller.api.log

import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.StatusCodes.{NotFound, OK}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.RouteTestTimeout
import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.delete
import java.nio.file.Path
import java.util.Objects.requireNonNull
import java.util.concurrent.ArrayBlockingQueue
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.implicits.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryFile
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.http.StreamingSupport.*
import js7.controller.web.controller.api.test.RouteTester
import monix.execution.Scheduler
import scala.concurrent.Future
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class LogRouteTest extends OurTestSuite with RouteTester with LogRoute:
  protected def whenShuttingDown = Future.never
  protected def currentLogFile = requireNonNull/*call lazily!*/(_currentLogFile)

  override protected def config = config"js7.web.server.services.log.poll-interval = 1.ms"
    .withFallback(super.config)

  implicit protected def scheduler = Scheduler.traced
  private implicit val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(99.s)

  private var _currentLogFile: Path = null

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
        // Akka testkit seems only support synchronous, blocking calls. So this blocks and fails:
        Get("/log") ~> Accept(`text/plain`) ~> route ~> check:
          assert(status == OK)
          val queue = new ArrayBlockingQueue[String](100)
          val completed = response.entity.dataBytes
            .toObservable
            .map(_.decodeString(UTF_8))
            .foreach(queue.add)
          assert(queue.poll(9, SECONDS) == "LOG TEXT")
          autoClosing(new OutputStreamWriter(new FileOutputStream(file, true))) { out =>
            for text <- Array("/ZWEI", "/DREI") do
              out.write(text)
              out.flush()
              assert(queue.poll(9, SECONDS) == text)
          }
          delete(file)
          completed await 99.s

      Get("log") ~> Accept(`text/plain`) ~> route ~> check:
        assert(status == NotFound && entityAs[String] == "The requested resource could not be found.")
    }
