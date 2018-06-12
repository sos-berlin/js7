package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.command.{CommandHandler, CommandMeta}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.base.auth.{SessionToken, ValidUserPermission}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import monix.execution.Scheduler

/**
 * @author Joacim Zschimmer
 */
trait CommandWebService extends AgentRouteProvider {

  protected def commandHandler: CommandHandler
  protected implicit def scheduler: Scheduler

  val commandRoute: Route =
    authorizedUser(ValidUserPermission) { user ⇒
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
