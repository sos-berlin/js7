package com.sos.jobscheduler.common.akkahttp.web.session

import akka.actor.{Actor, Props}
import com.sos.jobscheduler.base.auth.{SessionToken, User, UserId}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.common.akkahttp.web.session.SessionActor._
import com.sos.jobscheduler.common.auth.SecretStringGenerator
import com.sos.jobscheduler.common.scalautil.Logger
import monix.execution.{Cancelable, Scheduler}
import scala.collection.mutable
import scala.concurrent.duration._

/**
 * Executes public Agent commands.
 *
 * @author Joacim Zschimmer
 */
final class SessionActor[S <: LoginSession] private(newSession: SessionInit[S#User] ⇒ S, sessionTimeout: FiniteDuration)
  (implicit scheduler: Scheduler)
extends Actor {

  private val sessionTimeoutMillis = sessionTimeout.toMillis
  private val tokenToSession = mutable.Map[SessionToken, S]()
  private val numberIterator = Iterator.from(1)
  private var nextCleanUp: Cancelable = null

  def receive = {
    case Command.Login(user: S#User, tokenOption, isEternalSession) ⇒
      for (t ← tokenOption) delete(t, reason = "Login")
      val token = SessionToken(SecretStringGenerator.newSecretString())
      assert(!tokenToSession.contains(token), s"Duplicate generated SessionToken")  // Must not happen
      val session = newSession(SessionInit(token, numberIterator.next(), user))
      if (!isEternalSession) {
        session.timeoutAt = scheduler.currentTimeMillis + sessionTimeoutMillis
      }
      tokenToSession.insert(session.sessionToken → session)
      logger.info(s"Session #${session.sessionNumber} for User '${user.id}' added${if (session.isEternalSession) " (eternal)" else ""}")
      sender() ! token
      planNextCleanUp()

    case Command.Logout(token) ⇒
      delete(token, reason = "logout")
      sender() ! Completed

    case Command.Get(token, userId) ⇒
      val sessionOption = (tokenToSession.get(token), userId) match {
        case (None, _) ⇒
          logger.debug("Rejecting unknown session token" + userId.fold("")(o ⇒ s" (user '$o')"))
          None

        case (Some(s), Some(u)) if u != s.user.id ⇒
          logger.debug(s"Rejecting session token #${s.sessionNumber} belonging to user '${s.user.id}' but sent by user '$u'")
          None

        case (Some(s), _) ⇒
          if (handleTimeout(s))
            None
          else {
            if (s.timeoutAt != Never) {
              s.timeoutAt = scheduler.currentTimeMillis + sessionTimeoutMillis
            }
            Some(s)
          }
      }
      val checkedSession = sessionOption toChecked Problem("Invalid session token")
      sender() ! checkedSession

    case Command.GetCount ⇒
      sender() ! tokenToSession.size

    case CleanUp ⇒
      tokenToSession.values.toVector foreach handleTimeout
      nextCleanUp = null
      planNextCleanUp()
  }

  private def planNextCleanUp(): Unit =
    if (nextCleanUp == null && tokenToSession.values.exists(o ⇒ !o.isEternalSession)) {
      nextCleanUp = scheduler.scheduleOnce(CleanUpInterval) {
        self ! CleanUp
      }
    }

  private def handleTimeout(session: S): Boolean =
    if (scheduler.currentTimeMillis >= session.timeoutAt) {
      delete(session.sessionToken, reason = "timeout")
      true
    } else
      false

  private def delete(token: SessionToken, reason: String): Unit =
    tokenToSession.remove(token) foreach { session ⇒
      logger.info(s"Session #${session.sessionNumber} for User '${session.user.id}' deleted due to $reason")
    }
}

object SessionActor
{
  private val logger = Logger(getClass)
  private val Never = Long.MaxValue
  private val CleanUpInterval = 1.minute

  private[session] def props[S <: LoginSession](newSession: SessionInit[S#User] ⇒ S, sessionTimeout: FiniteDuration)(implicit s: Scheduler) =
    Props { new SessionActor[S](newSession, sessionTimeout) }

  private[session] sealed trait Command
  private[session] object Command {
    final case class Login[U <: User](user: U, oldSessionTokenOption: Option[SessionToken], isEternalSession: Boolean) extends Command
    final case class Logout(token: SessionToken) extends Command
    final case class Get(token: SessionToken, userId: Option[UserId]) extends Command
    final case object GetCount extends Command
  }

  private case object CleanUp
}
