package com.sos.jobscheduler.agent.web.views

import com.sos.jobscheduler.agent.views.AgentOverview
import com.sos.jobscheduler.agent.web.common.AgentWebService
import com.sos.jobscheduler.common.sprayutils.SprayJsonOrYamlSupport._
import scala.concurrent.{ExecutionContext, Future}
import spray.http.CacheDirectives.`max-age`
import spray.http.HttpHeaders.`Cache-Control`
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait RootWebService extends AgentWebService {

  protected def agentOverview: Future[AgentOverview]
  protected implicit def executionContext: ExecutionContext

  routeBuilder.addApiRoute { _ ⇒
    (pathEndOrSingleSlash | path("overview")) {
      get {
        respondWithHeader(`Cache-Control`(`max-age`(0))) {
          complete {
            agentOverview
          }
        }
      }
    }
  }
}
