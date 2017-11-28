package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import com.sos.jobscheduler.agent.web.common.AgentWebService
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.time.timer.TimerService
import scala.concurrent.ExecutionContext

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

object TimerWebService {
  intelliJuseImport(() ⇒ jsonOrYamlMarshaller(null))
}
