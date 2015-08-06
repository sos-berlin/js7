package com.sos.scheduler.engine.agent.web.views

import com.sos.scheduler.engine.agent.command.{CommandHandlerDetails, CommandHandlerOverview}
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.routing.Directives._

/**
  * @author Joacim Zschimmer
  */
trait CommandHandlerViewService extends ServiceStandards {

   protected def commandHandlerOverview: CommandHandlerOverview
   protected def commandHandlerDetails: CommandHandlerDetails

   addJobschedulerRoute {
     (pathPrefix("agent" / "commandHandler") & get) {
       respondWithHeader(`Cache-Control`(`max-age`(0))) {
         pathEnd {
           complete { commandHandlerOverview }
         } ~
         path("details") {
           complete { commandHandlerDetails }
         }
       }
     }
   }
 }
