package js7.tests.controller.proxy

import org.apache.pekko.http.scaladsl.common.JsonEntityStreamingSupport
import org.apache.pekko.http.scaladsl.marshalling.ToEntityMarshaller
import org.apache.pekko.http.scaladsl.server.Directives.{complete, get, pathSingleSlash}
import com.typesafe.config.ConfigFactory
import java.time.LocalDateTime
import js7.base.BuildInfo
import js7.base.auth.{Admission, UserAndPassword, UserId}
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.eventbus.StandardEventBus
import js7.base.generic.SecretString
import js7.base.log.Logger
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.Nel
import js7.base.web.Uri
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegments
import js7.common.pekkohttp.StandardMarshallers.monixObservableToMarshallable
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkoutils.Pekkos
import js7.common.commandline.CommandLineArguments
import js7.common.http.JsonStreamingSupport.{NdJsonStreamingSupport, jsonSeqMarshaller}
import js7.controller.client.PekkoHttpControllerApi
import js7.data.controller.ControllerState
import js7.data.event.{Event, EventId}
import js7.proxy.data.event.ProxyEvent
import js7.proxy.{ControllerApi, JournaledStateEventBus}
import js7.tests.controller.proxy.TestControllerProxy.*
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.Try

private final class TestControllerProxy(controllerUri: Uri, httpPort: Int)(implicit scheduler: Scheduler):
  def run(): Task[Unit] =
    Pekkos.actorSystemResource("TestControllerProxy")
      .use { implicit actorSystem =>
        val apiResource = PekkoHttpControllerApi.resource(Admission(controllerUri, userAndPassword))
        val proxyEventBus = new StandardEventBus[ProxyEvent]
        val eventBus = new JournaledStateEventBus[ControllerState]
        var currentState: ControllerState = null
        eventBus.subscribe[Event] { e => currentState = e.state }
        val api = new ControllerApi(apiResource map Nel.one)
        api.startProxy(proxyEventBus, eventBus)
          .flatMap { proxy =>
            PekkoWebServer
              .httpResource(httpPort, ConfigFactory.empty, webServiceRoute(Task(currentState)))
              .use(_ =>
                Task.tailRecM(())(_ =>
                  Task {
                    println(
                      Try(currentState).map(controllerState =>
                        EventId.toTimestamp(controllerState.eventId).show + " " +
                          controllerState.idToOrder.size + " orders: " +
                          controllerState.idToOrder.keys.take(5).map(_.string).mkString(", ")
                      ).fold(identity, identity))
                    Left(())
                  }.delayResult(1.s)))
          }
          .guarantee(api.stop)
      }


object TestControllerProxy:
  // Don't use a Logger here to avoid overwriting a concurrently used logfile
  private val userAndPassword = Some(UserAndPassword(UserId("demo"), SecretString("demo")))

  def main(args: Array[String]): Unit =
    implicit val scheduler: Scheduler = Scheduler.traced
    println(s"${LocalDateTime.now.toString.replace('T', ' ')} " +
      s"JS7 TestControllerProxy ${BuildInfo.longVersion}")
    Logger.initialize()
    CommandLineArguments.parse(args.toSeq) { arguments =>
      val controllerUri = arguments.as[Uri]("--controller-uri=")
      val httpPort = arguments.as[Int]("--http-port=")
      new TestControllerProxy(controllerUri, httpPort = httpPort)
        .run()
        .runSyncUnsafe()
    }

  private def webServiceRoute(snapshot: Task[ControllerState])(implicit s: Scheduler) =
    pathSegments("proxy/api/snapshot"):
      pathSingleSlash:
        get:
          complete(
            snapshot.map { controllerState =>
              implicit val x: JsonEntityStreamingSupport = NdJsonStreamingSupport
              implicit val y: TypedJsonCodec[Any] = ControllerState.snapshotObjectJsonCodec
              implicit val z: ToEntityMarshaller[Any] = jsonSeqMarshaller
              monixObservableToMarshallable(controllerState.toSnapshotObservable)
            }.runToFuture)
