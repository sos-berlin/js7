package com.sos.jobscheduler.agent.web.views

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import com.sos.jobscheduler.agent.views.AgentOverview
import com.sos.jobscheduler.agent.web.common.AgentWebService
import com.sos.jobscheduler.common.akkahttp.SprayJsonOrYamlSupport._
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Joacim Zschimmer
 */
trait RootWebService extends AgentWebService {

  protected def agentOverview: Future[AgentOverview]
  protected implicit def executionContext: ExecutionContext

  routeBuilder.addApiRoute { _ ⇒
    pathEndOrSingleSlash {
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
