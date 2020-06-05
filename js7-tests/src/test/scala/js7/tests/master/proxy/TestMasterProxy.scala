package js7.tests.master.proxy

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
import js7.core.web.StampedStreamingSupport.stampedCirceStreamingSupport
import js7.data.event.{Event, EventId}
import js7.master.client.AkkaHttpMasterApi
import js7.master.data.MasterSnapshots.SnapshotJsonCodec
import js7.master.data.MasterState
import js7.proxy.{JournaledProxy, ProxyEventBus}
import js7.tests.master.proxy.TestMasterProxy._
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.Try

private final class TestMasterProxy(masterUri: Uri, httpPort: Int)(implicit scheduler: Scheduler)
{
  def run(): Task[Unit] =
    Akkas.actorSystemResource("TestMasterProxy")
      .use { implicit actorSystem =>
        val apiResource = AkkaHttpMasterApi.resource(masterUri, userAndPassword)
        val eventBus = new ProxyEventBus[MasterState]
        var currentState: (EventId, MasterState) = null
        eventBus.subscribe[Event] { e => currentState = e.stampedEvent.eventId -> e.state }
        JournaledProxy.start[MasterState](apiResource, eventBus.publish)
          .flatMap { proxy =>
            AkkaWebServer.resourceForHttp(httpPort, webServiceRoute(Task(currentState)))
              .use(_ =>
                Task.tailRecM(())(_ =>
                  Task {
                    println(
                      Try(currentState).map { case (eventId, masterState) =>
                        EventId.toTimestamp(eventId).show + " " +
                          masterState.idToOrder.size + " orders: " + (masterState.idToOrder.keys.take(5).map(_.string).mkString(", "))
                      }.fold(identity, identity))
                    Left(())
                  }.delayResult(1.s))
            )
          }
      }
}

object TestMasterProxy
{
  // Don't use a Logger here to avoid overwriting a concurrently used logfile
  private val userAndPassword = Some(UserAndPassword(UserId("demo"), SecretString("demo")))

  def main(args: Array[String]): Unit = {
    implicit def scheduler = Scheduler.global
    println(s"${LocalDateTime.now.toString.replace('T', ' ')} JS7 TestMasterProxy ${BuildInfo.prettyVersion}")
    ScribeUtils.coupleScribeWithSlf4j()
    CommandLineArguments.parse(args.toSeq) { arguments =>
      val masterUri = arguments.as[Uri]("--master-uri=")
      val httpPort = arguments.as[Int]("--http-port=")
      new TestMasterProxy(masterUri, httpPort = httpPort).run()
        .runSyncUnsafe()
    }
  }

  private def webServiceRoute(snapshot: Task[(EventId, MasterState)])(implicit s: Scheduler) =
    pathSegments("proxy/api/snapshot") {
      pathSingleSlash {
        get {
          complete(
            snapshot.map { case (eventId, masterState) =>
              implicit val x = stampedCirceStreamingSupport(eventId = eventId)
              implicit val y = SnapshotJsonCodec
              monixObservableToMarshallable(masterState.toSnapshotObservable)
            }.runToFuture)
        }
      }
    }
}
