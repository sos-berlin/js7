package com.sos.jobscheduler.agent.command

import akka.actor.Actor
import com.sos.jobscheduler.agent.command.SessionActor._
import com.sos.jobscheduler.agent.data.commandresponses.{EmptyResponse, LoginResponse}
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{Login, Logout, Response, SessionCommand}
import com.sos.jobscheduler.agent.web.common.LoginSession
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRegister
import com.sos.jobscheduler.common.auth.User
import com.sos.jobscheduler.data.session.SessionToken
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

/**
 * Executes public Agent commands.
 *
 * @author Joacim Zschimmer
 */
final class SessionActor(sessionRegister: SessionRegister[LoginSession])(implicit ec: ExecutionContext)
extends Actor {

  def receive = {
    case Command.Execute(command, meta, response) ⇒
      response completeWith executeCommand(command, meta)
  }

  private def executeCommand(command: SessionCommand, meta: CommandMeta): Future[Response] =
    command match {
      case Login ⇒ login(meta.sessionTokenOption, meta.user)
      case Logout ⇒ logout(meta.sessionTokenOption)
    }

  private def login(currentSessionTokenOption: Option[SessionToken], user: User) =
    Future fromTry Try {
      currentSessionTokenOption foreach sessionRegister.remove
      LoginResponse(sessionRegister.add(LoginSession(user)))
    }

  private def logout(sessionTokenOption: Option[SessionToken]) =
    Future fromTry Try {
      val sessionToken = sessionTokenOption getOrElse {
        throw new IllegalArgumentException("Logout without SessionToken")
      }
      sessionRegister.remove(sessionToken)
      EmptyResponse
    }
}

object SessionActor {
  object Command {
    final case class Execute(command: SessionCommand, meta: CommandMeta, response: Promise[Response])
  }
}
