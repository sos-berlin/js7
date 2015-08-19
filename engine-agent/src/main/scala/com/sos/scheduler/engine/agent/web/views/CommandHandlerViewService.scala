package com.sos.scheduler.engine.agent.web.views

import com.sos.scheduler.engine.agent.command.{CommandHandlerOverview, CommandRunOverview}
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import com.sos.scheduler.engine.common.utils.IntelliJUtils._
import scala.collection.immutable
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.json.DefaultJsonProtocol._
import spray.routing.Directives._

/**
  * @author Joacim Zschimmer
  */
trait CommandHandlerViewService extends ServiceStandards {

  protected def commandHandlerOverview: CommandHandlerOverview
  protected def commandRunOverviews: immutable.Iterable[CommandRunOverview]

  addApiRoute {
    (pathPrefix("command") & get) {
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

object CommandHandlerViewService {
  intelliJuseImports(rootFormat _)
}
