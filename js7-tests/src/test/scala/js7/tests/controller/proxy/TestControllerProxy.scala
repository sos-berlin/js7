package js7.tests.controller.proxy

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.flatMap.*
import com.typesafe.config.ConfigFactory
import fs2.Stream
import io.circe.syntax.EncoderOps
import java.time.LocalDateTime
import js7.base.BuildInfo
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.catsutils.OurIORuntime
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.eventbus.StandardEventBus
import js7.base.fs2utils.StreamExtensions.mapParallelBatch
import js7.base.generic.SecretString
import js7.base.log.Logger
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsBlocking.*
import js7.base.utils.CatsUtils.Nel
import js7.base.web.Uri
import js7.common.commandline.CommandLineArguments
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.pekkohttp.PekkoHttpServerUtils.{completeWithStream, pathSegments}
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkoutils.ByteStrings.syntax.ByteStringToByteSequence
import js7.common.pekkoutils.Pekkos
import js7.controller.client.PekkoHttpControllerApi
import js7.data.controller.ControllerState
import js7.data.event.{Event, EventId}
import js7.proxy.data.event.ProxyEvent
import js7.proxy.{ControllerApi, JournaledStateEventBus}
import js7.tests.controller.proxy.TestControllerProxy.*
import org.apache.pekko.http.scaladsl.server.Directives.{get, pathSingleSlash}
import scala.util.Try

private final class TestControllerProxy(controllerUri: Uri, httpPort: Int)
  (implicit ioRuntime: IORuntime):

  def run(): IO[Unit] =
    Pekkos.actorSystemResource("TestControllerProxy").use { implicit actorSystem =>
      val apiResource = PekkoHttpControllerApi.resource(Admission(controllerUri, userAndPassword))
      val proxyEventBus = new StandardEventBus[ProxyEvent]
      val eventBus = new JournaledStateEventBus[ControllerState]
      var currentState: ControllerState | Null = null
      eventBus.subscribe[Event]: e =>
        currentState = e.state
      val api = new ControllerApi(apiResource map Nel.one)
      api.controllerProxy(proxyEventBus, eventBus).blockingUse(99.s): proxy =>
        PekkoWebServer
          .httpResource(httpPort, ConfigFactory.empty, webServiceRoute(IO(currentState.nn)))
          .use: _ =>
            ().tailRecM: _ =>
              IO:
                println:
                  Try(currentState.nn).map: controllerState =>
                    EventId.toTimestamp(controllerState.eventId).show + " " +
                      controllerState.idToOrder.size + " orders: " +
                      controllerState.idToOrder.keys.take(5).map(_.string).mkString(", ")
                  .fold(identity, identity)
                Left(())
              .andWait(1.s)
      .guarantee:
        api.stop
    }


object TestControllerProxy:
  // Don't use a Logger here to avoid overwriting a concurrently used logfile
  private val userAndPassword = Some(UserAndPassword(UserId("demo"), SecretString("demo")))

  def main(args: Array[String]): Unit =
    given IORuntime = OurIORuntime.commonIORuntime
    println(s"${LocalDateTime.now.toString.replace('T', ' ')} " +
      s"JS7 TestControllerProxy ${BuildInfo.prettyVersion}")
    Logger.initialize("JS7 TestControllerProxy")
    CommandLineArguments.parse(args.toSeq): arguments =>
      val controllerUri = arguments.as[Uri]("--controller-uri=")
      val httpPort = arguments.as[Int]("--http-port=")
      new TestControllerProxy(controllerUri, httpPort = httpPort)
        .run()
        .unsafeRunSync()

  private def webServiceRoute(snapshot: IO[ControllerState])(using IORuntime) =
    pathSegments("proxy/api/snapshot"):
      pathSingleSlash:
        get:
          completeWithStream(`application/x-ndjson`):
            Stream.eval(snapshot).flatMap: controllerState =>
              given TypedJsonCodec[Any] = ControllerState.snapshotObjectJsonCodec
              controllerState.toSnapshotStream
                .mapParallelBatch():
                  _.asJson.toByteArray.toByteString
