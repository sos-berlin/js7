package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import com.sos.jobscheduler.agent.command.{CommandHandler, CommandMeta}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.web.common.AgentWebService
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.SprayJsonOrYamlSupport._
import com.sos.jobscheduler.data.session.SessionToken
import scala.concurrent.ExecutionContext
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
trait CommandWebService extends AgentWebService {

  protected def commandHandler: CommandHandler
  protected implicit def executionContext: ExecutionContext

  routeBuilder.addApiRoute { user ⇒
    pathPrefix("command") {
      post {
        pathEnd {
          optionalHeaderValueByName(SessionToken.HeaderName) { sessionTokenOption ⇒
            entity(as[AgentCommand]) { command ⇒
              complete {
                commandHandler.execute(command, CommandMeta(
                  user,
                  sessionTokenOption map { o ⇒ SessionToken(SecretString(o)) }))
                }
            }
          }
        }
      } ~
      get {
        respondWithHeader(`Cache-Control`(`max-age`(0))) {
          pathEnd {
            complete { commandHandler.overview }
          } ~
          pathSingleSlash {
            complete { commandHandler.detailed map { _.commandRuns } }
          }
        }
      }
    }
  }
}
