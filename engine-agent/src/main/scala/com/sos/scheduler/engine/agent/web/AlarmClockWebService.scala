package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.web.common.AgentWebService
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.scheduler.engine.common.time.alarm.AlarmClock
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.routing.Directives._
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
trait AlarmClockWebService extends AgentWebService {

  protected def alarmClock: AlarmClock

  addApiRoute {
    pathPrefix("alarmClock") {
      respondWithHeader(`Cache-Control`(`max-age`(0))) {
        pathEnd {
          get {
            complete { alarmClock.overview }
          }
        } ~
        pathSingleSlash {
          get {
            complete { alarmClock.alarmOverviews }
          }
        }
      }
    }
  }
}
