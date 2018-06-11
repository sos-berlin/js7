package com.sos.jobscheduler.common.akkahttp.web.session

import akka.actor.{Actor, Props}
import com.sos.jobscheduler.base.auth.{SessionToken, User, UserId}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.akkahttp.web.session.SessionActor._
import com.sos.jobscheduler.common.auth.SecretStringGenerator
import com.sos.jobscheduler.common.scalautil.Logger
import scala.collection.mutable

/**
 * Executes public Agent commands.
 *
 * @author Joacim Zschimmer
 */
final class SessionActor[S <: LoginSession] private(newSession: SessionInit[S#User] ⇒ S)
extends Actor {

  private val sessions = mutable.Map[SessionToken, S]()
  private val numberIterator = Iterator.from(1)

  // TODO Delete session when not used for some time

  def receive = {
    case Command.Login(user: S#User, sessionTokenOption) ⇒
      sessionTokenOption foreach delete
      val token = SessionToken(SecretStringGenerator.newSecretString())
      assert(!sessions.contains(token), s"Duplicate generated SessionToken")  // Must not happen
      val session = newSession(SessionInit(token, numberIterator.next(), user))
      sessions.put(token, session)
      logger.info(s"User '${user.id}': LoginSession #${session.sessionNumber} added")
      sender() ! token

    case Command.Logout(sessionToken) ⇒
      delete(sessionToken)
      sender() ! Completed

    case Command.Get(sessionToken, userId) ⇒
      val sessionOption = (sessions.get(sessionToken), userId) match {
        case (None, _) ⇒
          logger.debug("Rejecting unknown session token" + userId.fold("")(o ⇒ s" (user '$o')"))
          None
        case (Some(s), Some(u)) if u != s.user.id ⇒
          logger.debug(s"Rejecting session token belonging to user '${s.user.id}' sent by user '$u'")
          None
        case (Some(s), _) ⇒
          Some(s)
      }
      val checkedSession = sessionOption toChecked Problem("Invalid session token")
      sender() ! checkedSession
  }

  private def delete(sessionToken: SessionToken): Unit =
    sessions.remove(sessionToken) foreach {
      session ⇒ logger.info(s"User '${session.user.id}': LoginSession #${session.sessionNumber} deleted")
    }
}

object SessionActor
{
  private val logger = Logger(getClass)

  private[session] def props[S <: LoginSession](newSession: SessionInit[S#User] ⇒ S) =
    Props { new SessionActor[S](newSession) }

  private[session] sealed trait Command
  private[session] object Command {
    final case class Login[U <: User](user: U, oldSessionTokenOption: Option[SessionToken]) extends Command
    final case class Logout(sessionToken: SessionToken) extends Command
    final case class Get(sessionToken: SessionToken, userId: Option[UserId]) extends Command
  }
}
