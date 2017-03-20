package com.sos.jobscheduler.master.web.api

import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.jobscheduler.common.sprayutils.SprayUtils.pathSegments
import com.sos.jobscheduler.master.web.simplegui.FrontEndRoute
import spray.json.DefaultJsonProtocol._
import spray.routing.Directives._
import spray.routing.Route

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute extends OrderRoute with FrontEndRoute {

  private case class MasterConfiguration(hello: String, version: String)
  private object MasterConfiguration {
    implicit val jsonFormat = jsonFormat2(apply)
  }

  def apiRoute: Route =
    pathEnd {
     complete(MasterConfiguration(
       hello = "This is the new experimental *** JobScheduler Master ***",
       version = BuildInfo.buildVersion))
    } ~
    pathSegments("order") {
      orderRoute
    } ~
    pathSegments("frontend") {
      frontEndRoute
    }
}
