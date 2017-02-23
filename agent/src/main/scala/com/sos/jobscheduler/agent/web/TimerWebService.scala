package com.sos.jobscheduler.agent.web

import com.sos.jobscheduler.agent.web.common.AgentWebService
import com.sos.jobscheduler.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.jobscheduler.common.sprayutils.SprayUtils.pathSegments
import com.sos.jobscheduler.common.time.timer.TimerService
import scala.concurrent.ExecutionContext
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.json.DefaultJsonProtocol._
import spray.routing.Directives._

/**
  * @author Joacim Zschimmer
  */
trait TimerWebService extends AgentWebService {

  protected def timerService: TimerService
  protected implicit def executionContext: ExecutionContext

  routeBuilder.addApiRoute { _ ⇒
    pathSegments("timer") {
      respondWithHeader(`Cache-Control`(`max-age`(0))) {
        pathEnd {
          get {
            complete { timerService.overview }
          }
        } ~
        pathSingleSlash {
          get {
            complete { timerService.timerOverviews }
          }
        }
      }
    }
  }
}
