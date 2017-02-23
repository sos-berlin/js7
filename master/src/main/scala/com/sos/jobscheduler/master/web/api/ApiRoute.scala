package com.sos.scheduler.engine.master.web.api

import com.sos.scheduler.engine.common.BuildInfo
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.scheduler.engine.common.sprayutils.SprayUtils.pathSegments
import spray.json.DefaultJsonProtocol._
import spray.routing.Directives._
import spray.routing.Route

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute extends OrderRoute {

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
    }
}
