package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.command.CommandMeta
import com.sos.jobscheduler.agent.data.command.{CommandHandlerDetailed, CommandHandlerOverview}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.base.auth.{SessionToken, ValidUserPermission}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import monix.eval.Task

/**
 * @author Joacim Zschimmer
 */
trait CommandWebService extends AgentRouteProvider {

  protected def commandExecute(meta: CommandMeta, command: AgentCommand): Task[AgentCommand.Response]
  protected def commandOverview: Task[CommandHandlerOverview]
  protected def commandDetailed: Task[CommandHandlerDetailed]

  private implicit def implicitScheduler = scheduler

  final lazy val commandRoute: Route =
    authorizedUser(ValidUserPermission) { user ⇒
      post {
        pathEnd {
          optionalHeaderValueByName(SessionToken.HeaderName) { sessionTokenOption ⇒
            entity(as[AgentCommand]) { command ⇒
              complete {
                val meta = CommandMeta(user, sessionTokenOption map { o ⇒ SessionToken(SecretString(o)) })
                commandExecute(meta, command)
              }
            }
          }
        }
      } ~
      get {
        respondWithHeader(`Cache-Control`(`max-age`(0))) {
          pathEnd {
            complete { commandOverview }
          } ~
          pathSingleSlash {
            complete { commandDetailed map { _.commandRuns } }
          }
        }
      }
    }
}
