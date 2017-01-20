package com.sos.scheduler.engine.http.server.heartbeat

import com.sos.scheduler.engine.http.client.heartbeat.HeartbeatRequestHeaders.`X-JobScheduler-Heartbeat`
import java.time.Duration
import spray.http.HttpResponse
import spray.http.StatusCodes._
import spray.routing.Directives._
import spray.routing._

/**
  * @author Joacim Zschimmer
  */
object ClientSideHeartbeatService {
  def clientSideHeartbeat(body: Duration ⇒ Unit): Route =
    headerValueByName(`X-JobScheduler-Heartbeat`.name) { case `X-JobScheduler-Heartbeat`.Value(times) ⇒
      requestEntityEmpty {
        body(times.timeout)
        complete(HttpResponse(OK))
      } ~
        complete((BadRequest, "Heartbeat with payload?"))
    }
}
