package com.sos.jobscheduler.tests.master.proxy

import akka.http.scaladsl.server.Directives.{complete, get, pathSingleSlash}
import com.sos.jobscheduler.base.BuildInfo
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers.monixObservableToMarshallable
import com.sos.jobscheduler.common.akkahttp.web.AkkaWebServer
import com.sos.jobscheduler.common.akkautils.Akkas
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.log.ScribeUtils
import com.sos.jobscheduler.core.web.StampedStreamingSupport.stampedCirceStreamingSupport
import com.sos.jobscheduler.data.event.{Event, EventId}
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.master.data.MasterSnapshots.SnapshotJsonCodec
import com.sos.jobscheduler.master.data.MasterState
import com.sos.jobscheduler.proxy.{JournaledProxy, ProxyEventBus}
import com.sos.jobscheduler.tests.master.proxy.TestMasterProxy._
import java.time.LocalDateTime
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
    println(s"${LocalDateTime.now.toString.replace('T', ' ')} JobScheduler TestMasterProxy ${BuildInfo.prettyVersion}")
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
