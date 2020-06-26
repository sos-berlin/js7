package js7.controller.web.controller.api.log

import akka.http.scaladsl.model.MediaTypes.`text/plain`
import akka.http.scaladsl.model.StatusCodes.{NotFound, OK}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.RouteTestTimeout
import com.typesafe.config.ConfigFactory
import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.delete
import java.nio.file.Path
import java.util.Objects.requireNonNull
import java.util.concurrent.ArrayBlockingQueue
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.http.StreamingSupport._
import js7.common.scalautil.FileUtils.implicits._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.withTemporaryFile
import js7.common.scalautil.Futures.implicits._
import js7.controller.web.controller.api.test.RouteTester
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class LogRouteTest extends AnyFreeSpec with RouteTester with LogRoute
{
  protected def isShuttingDown = false
  protected def currentLogFile = requireNonNull/*call lazily!*/(_currentLogFile)

  override protected def config = ConfigFactory.parseString(
    """js7.webserver.services.log.poll-interval = 1.ms
      |""".stripMargin)
    .withFallback(super.config)

  implicit protected def scheduler = Scheduler.global
  private implicit val routeTestTimeout = RouteTestTimeout(9.seconds)

  private var _currentLogFile: Path = null

  private lazy val route = seal(
    pathSegment("log") {
      logRoute
    })

  "/controller/api/log" in {
    withTemporaryFile("LogRouteTest", ".tmp") { file =>
      _currentLogFile = file

      file := "LOG TEXT"
      Get("/log?snapshot=true") ~> Accept(`text/plain`) ~> route ~> check {
        assert(status == OK && entityAs[String] == "LOG TEXT")
      }

      if (false) {
        // Endless streaming response
        // Akka testkit seems only support synchronous, blocking calls. So this blocks and fails:
        Get("/log") ~> Accept(`text/plain`) ~> route ~> check {
          assert(status == OK)
          val queue = new ArrayBlockingQueue[String](100)
          val completed = response.entity.dataBytes
            .toObservable
            .map(_.decodeString(UTF_8))
            .foreach(queue.add)
          assert(queue.poll(9, SECONDS) == "LOG TEXT")
          autoClosing(new OutputStreamWriter(new FileOutputStream(file, true))) { out =>
            for (text <- Array("/ZWEI", "/DREI")) {
              out.write(text)
              out.flush()
              assert(queue.poll(9, SECONDS) == text)
            }
          }
          delete(file)
          completed await 99.s
        }
      }

      Get("log") ~> Accept(`text/plain`) ~> route ~> check {
        assert(status == NotFound && entityAs[String] == "The requested resource could not be found.")
      }
    }
  }
}
