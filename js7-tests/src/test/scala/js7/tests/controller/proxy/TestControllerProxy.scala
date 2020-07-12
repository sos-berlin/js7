package js7.tests.controller.proxy

import akka.http.scaladsl.server.Directives.{complete, get, pathSingleSlash}
import java.time.LocalDateTime
import js7.base.BuildInfo
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.common.akkahttp.StandardMarshallers.monixObservableToMarshallable
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkautils.Akkas
import js7.common.commandline.CommandLineArguments
import js7.common.log.ScribeUtils
import js7.controller.client.AkkaHttpControllerApi
import js7.controller.data.ControllerSnapshots.SnapshotJsonCodec
import js7.controller.data.ControllerState
import js7.core.web.StampedStreamingSupport.stampedCirceStreamingSupport
import js7.data.event.{Event, EventId}
import js7.proxy.javaapi.JStandardEventBus
import js7.proxy.{JournaledProxy, JournaledStateEventBus, ProxyEvent}
import js7.tests.controller.proxy.TestControllerProxy._
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.Try

private final class TestControllerProxy(controllerUri: Uri, httpPort: Int)(implicit scheduler: Scheduler)
{
  def run(): Task[Unit] =
    Akkas.actorSystemResource("TestControllerProxy")
      .use { implicit actorSystem =>
        val apiResource = AkkaHttpControllerApi.resource(controllerUri, userAndPassword)
        val proxyEventBus = new JStandardEventBus[ProxyEvent]
        val eventBus = new JournaledStateEventBus[ControllerState]
        var currentState: (EventId, ControllerState) = null
        eventBus.subscribe[Event] { e => currentState = e.stampedEvent.eventId -> e.state }
        JournaledProxy.start[ControllerState](apiResource, proxyEventBus.underlying.publish, eventBus.publish)
          .flatMap { proxy =>
            AkkaWebServer.resourceForHttp(httpPort, webServiceRoute(Task(currentState)))
              .use(_ =>
                Task.tailRecM(())(_ =>
                  Task {
                    println(
                      Try(currentState).map { case (eventId, controllerState) =>
                        EventId.toTimestamp(eventId).show + " " +
                          controllerState.idToOrder.size + " orders: " + (controllerState.idToOrder.keys.take(5).map(_.string).mkString(", "))
                      }.fold(identity, identity))
                    Left(())
                  }.delayResult(1.s))
            )
          }
      }
}

object TestControllerProxy
{
  // Don't use a Logger here to avoid overwriting a concurrently used logfile
  private val userAndPassword = Some(UserAndPassword(UserId("demo"), SecretString("demo")))

  def main(args: Array[String]): Unit = {
    implicit def scheduler = Scheduler.global
    println(s"${LocalDateTime.now.toString.replace('T', ' ')} JS7 TestControllerProxy ${BuildInfo.prettyVersion}")
    ScribeUtils.coupleScribeWithSlf4j()
    CommandLineArguments.parse(args.toSeq) { arguments =>
      val controllerUri = arguments.as[Uri]("--controller-uri=")
      val httpPort = arguments.as[Int]("--http-port=")
      new TestControllerProxy(controllerUri, httpPort = httpPort).run()
        .runSyncUnsafe()
    }
  }

  private def webServiceRoute(snapshot: Task[(EventId, ControllerState)])(implicit s: Scheduler) =
    pathSegments("proxy/api/snapshot") {
      pathSingleSlash {
        get {
          complete(
            snapshot.map { case (eventId, controllerState) =>
              implicit val x = stampedCirceStreamingSupport(eventId = eventId)
              implicit val y = SnapshotJsonCodec
              monixObservableToMarshallable(controllerState.toSnapshotObservable)
            }.runToFuture)
        }
      }
    }
}
