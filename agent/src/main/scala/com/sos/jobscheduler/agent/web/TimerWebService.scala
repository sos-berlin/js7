package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.time.timer.TimerService
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait TimerWebService extends AgentRouteProvider {

  protected def timerService: TimerService
  protected implicit def scheduler: Scheduler

  protected final val timerRoute: Route =
    authorizedUser() { _ ⇒
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

object TimerWebService {
  intelliJuseImport(() ⇒ jsonOrYamlMarshaller(null))
}
