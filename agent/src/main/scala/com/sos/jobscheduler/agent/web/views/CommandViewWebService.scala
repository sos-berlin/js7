package com.sos.jobscheduler.agent.web.views

import com.sos.jobscheduler.agent.command.{CommandHandlerOverview, CommandRunOverview}
import com.sos.jobscheduler.agent.web.common.AgentWebService
import com.sos.jobscheduler.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.jobscheduler.common.sprayutils.SprayUtils.pathSegments
import com.sos.jobscheduler.common.utils.IntelliJUtils._
import scala.collection.immutable
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.json.DefaultJsonProtocol._
import spray.routing.Directives._

/**
  * @author Joacim Zschimmer
  */
trait CommandViewWebService extends AgentWebService {

  protected def commandHandlerOverview: CommandHandlerOverview
  protected def commandRunOverviews: immutable.Iterable[CommandRunOverview]

  routeBuilder.addApiRoute { _ ⇒
    (pathSegments("command") & get) {
      respondWithHeader(`Cache-Control`(`max-age`(0))) {
        pathEnd {
         complete { commandHandlerOverview }
        } ~
        pathSingleSlash {
          complete { commandRunOverviews }
        }
      }
    }
  }
}

object CommandViewWebService {
  intelliJuseImports(rootFormat _)
}
