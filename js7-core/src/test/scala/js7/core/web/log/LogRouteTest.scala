package js7.core.web.log

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO}
import io.circe.Encoder
import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{createTempFile, delete, deleteIfExists}
import java.nio.file.Path
import java.time.Instant
import java.util.concurrent.ArrayBlockingQueue
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.common.http.StreamingSupport.*
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegment
import js7.common.pekkohttp.web.session.SimpleSession
import js7.core.web.log.LogRoute
import js7.core.web.log.LogRoute.stringToInstant
import js7.core.web.test.RouteTester
import js7.data.node.EngineServerId
import org.apache.pekko.http.scaladsl.model.MediaTypes.`text/plain`
import org.apache.pekko.http.scaladsl.model.StatusCodes.OK
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.testkit.RouteTestTimeout
import scala.concurrent.duration.*


/**
  * @author Joacim Zschimmer
  */
final class LogRouteTest extends OurTestSuite, RouteTester, LogRoute:

  protected type OurSession = SimpleSession
  protected val sessionEncoder = summon[Encoder.AsObject[SimpleSession]]
  protected def whenShuttingDown = Deferred.unsafe
  protected lazy val logFile: Path = createTempFile("LogRouteTest-", ".log")
  protected val logDirectory = logFile.getParent
  protected val engineServerId = IO.right(EngineServerId.primaryController)
  protected def logDirectoryIndexRegister = throw new AssertionError("logDirectoryIndexRegister")

  override protected def config = config"""
    js7.log.info.file = "${logFile.getFileName}"
    js7.log.debug.file = "${logFile.getFileName}"
    js7.web.server.services.log.poll-interval = 1.ms
    js7.web.chunk-size = 16
    """.withFallback(super.config)


  private given IORuntime = ioRuntime

  override def afterAll() =
    try
      deleteIfExists(logFile)
    finally
      super.afterAll()

  private lazy val route =
    seal:
      pathSegment("log"):
        logRoute

  "stringToInstant" in:
    assert:
      stringToInstant("2026-02-12T20:21:22.123456789Z") ==
        Right(Instant.parse("2026-02-12T20:21:22.123456789Z"))
    assert:
      stringToInstant("2026-02-12T20:21:22.123456Z") ==
        Right(Instant.parse("2026-02-12T20:21:22.123456Z"))
    assert:
      stringToInstant("2026-02-12T20:21:22.123Z") ==
        Right(Instant.parse("2026-02-12T20:21:22.123Z"))
    assert:
      stringToInstant("2026-02-12T20:21:22.1Z") ==
        Right(Instant.parse("2026-02-12T20:21:22.1Z"))
    assert:
      stringToInstant("2026-02-12T20:21:22Z") ==
        Right(Instant.parse("2026-02-12T20:21:22Z"))
    assert:
      stringToInstant("2026-02-12T20:21:22,123Z") ==
        Right(Instant.parse("2026-02-12T20:21:22.123Z"))
    assert:
      stringToInstant("2026-02-12T22:21:22.123456789+02:00") ==
        Right(Instant.parse("2026-02-12T20:21:22.123456789Z"))
    assert:
      stringToInstant("2026-02-12T22:21:22+02:00") ==
        Right(Instant.parse("2026-02-12T20:21:22Z"))
    //assert:
    //  stringToInstant("2026-02-12T22:21:22.123456789[Europe/Mariehamn]") ==
    //    Right(Instant.parse("2026-02-12T20:21:22.123456789Z"))

  "/controller/api/log/info/raw" in:
    logFile := "LOG TEXT"
    Get("/log/info/raw") ~> Accept(`text/plain`) ~> route ~> check:
      assert(status == OK && entityAs[String] == "LOG TEXT")

  "/controller/api/log growing" in:
    pending

    // Endless streaming response
    // Pekko testkit seems only support synchronous, blocking calls. So this blocks and fails !!!
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
      autoClosing(new OutputStreamWriter(new FileOutputStream(logFile.toFile, true))) { out =>
        for text <- Array("/ZWEI", "/DREI") do
          out.write(text)
          out.flush()
          assert(queue.poll(9, SECONDS) == text)
      }
      delete(logFile)
      completed.await(99.s)

    //Get("log") ~> Accept(`text/plain`) ~> route ~> check:
    //  assert(status == NotFound && entityAs[String] == "The requested resource could not be found.")
