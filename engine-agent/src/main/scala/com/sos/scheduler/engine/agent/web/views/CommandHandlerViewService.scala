package com.sos.scheduler.engine.agent.web.views

import com.sos.scheduler.engine.agent.command.{CommandHandlerDetails, CommandHandlerOverview}
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.agent.web.marshal.SprayJsonOrYamlSupport._
import spray.routing.Directives._

/**
  * @author Joacim Zschimmer
  */
trait CommandHandlerViewService extends ServiceStandards {

   protected def commandHandlerOverview: CommandHandlerOverview
   protected def commandHandlerDetails: CommandHandlerDetails

   addRoute {
     (get & pathPrefix("agent" / "commandHandler")) {
       pathEnd {
         complete { commandHandlerOverview }
       } ~
       path("details") {
         complete { commandHandlerDetails }
       }
     }
   }
 }
