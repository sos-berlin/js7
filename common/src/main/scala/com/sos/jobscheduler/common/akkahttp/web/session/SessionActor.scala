package com.sos.jobscheduler.common.akkahttp.web.session

import akka.actor.{Actor, DeadLetterSuppression, Props}
import com.sos.jobscheduler.base.auth.{SessionToken, User}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.common.akkahttp.web.session.SessionActor._
import com.sos.jobscheduler.common.auth.SecretStringGenerator
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.typesafe.config.Config
import monix.execution.{Cancelable, Scheduler}
import scala.collection.mutable

// TODO https://www.owasp.org/index.php/Session_Management_Cheat_Sheet
/**
 * Executes public Agent commands.
 *
 * @author Joacim Zschimmer
 */
final class SessionActor[S <: Session] private(newSession: SessionInit[S#User] ⇒ S, config: Config)
  (implicit scheduler: Scheduler)
extends Actor {

  private val sessionTimeout = config.getDuration("jobscheduler.auth.session.timeout").toFiniteDuration
  private val cleanupInterval = sessionTimeout / 4
  private val tokenToSession = mutable.Map[SessionToken, S]()
  private val numberIterator = Iterator.from(1)
  private var nextCleanup: Cancelable = null

  override def postStop(): Unit = {
    if (nextCleanup != null) nextCleanup.cancel()
    super.postStop()
  }

  def receive = {
    case Command.Login(user: S#User, tokenOption, isEternalSession) ⇒
      for (t ← tokenOption) delete(t, reason = "secondlogin")
      val token = SessionToken(SecretStringGenerator.newSecretString())
      assert(!tokenToSession.contains(token), s"Duplicate generated SessionToken")  // Must not happen
      val session = newSession(SessionInit(token, numberIterator.next(), user))
      if (!isEternalSession) {
        session.touch(sessionTimeout)
      }
      tokenToSession.insert(session.sessionToken → session)
      logger.info(s"Session #${session.sessionNumber} for user '${user.id}' added${if (session.isEternal) " (eternal)" else ""}")
      sender() ! token
      scheduleNextCleanup()

    case Command.Logout(token) ⇒
      delete(token, reason = "logout")
      sender() ! Completed

    case Command.Get(token, userOption: Option[S#User]) ⇒
      val sessionOption = (tokenToSession.get(token), userOption) match {
        case (None, _) ⇒
          logger.debug("Rejecting unknown session token" + userOption.fold("")(o ⇒ s" (user '${o.id.string}')"))
          None

        case (Some(session), Some(user)) if user.id != session.currentUser.id ⇒
          tryUpdateLatelyAuthenticatedUser(user, session)

        case (Some(session), _) ⇒
          if (handleTimeout(session))
            None
          else {
            if (!session.isEternal) {
              session.touch(sessionTimeout)
            }
            Some(session)
          }
      }
      val checkedSession = sessionOption toChecked Problem("Invalid session token")
      sender() ! checkedSession

    case Command.GetCount ⇒
      sender() ! tokenToSession.size

    case CleanUp ⇒
      logger.trace("CleanUp")
      tokenToSession.values.toVector foreach handleTimeout
      nextCleanup = null
      scheduleNextCleanup()
  }

  /** Handles late authentication for Browser usage.
    * login(None) was accepted without authentication, establishing a Session for Anonymous.
    * A web service calling authorize() may require more rights than Anonymous,
    * leading to 401 and browser authentication dialog.
    * The resent (current) request has a HTTP authentication header, which was authentiated (see caller)
    * The session is updated with the authenticated user.
    * This may happen only once and the original user must be Anonymous.
    */
  private def tryUpdateLatelyAuthenticatedUser(newUser: S#User, session: Session): Option[Session] = {
    if (session.sessionInit.originalUser.isAnonymous &&
        session.tryUpdateUser(newUser.asInstanceOf[session.User]))  // Mutate session!
    {
      logger.info(s"Session #${session.sessionNumber} for user '${session.sessionInit.originalUser.id}' changed to user '${newUser.id}'")
      Some(session)
    } else {
      logger.debug(s"Rejecting session token #${session.sessionNumber} belonging to user '${session.currentUser.id}' but sent by user '${newUser.id.string}'")
      None
    }
  }

  private def scheduleNextCleanup(): Unit =
    if (nextCleanup == null && tokenToSession.values.exists(o ⇒ !o.isEternal)) {
      nextCleanup = scheduler.scheduleOnce(cleanupInterval) {
        self ! CleanUp
      }
    }

  private def handleTimeout(session: S): Boolean =
    if (!session.isAlive) {
      delete(session.sessionToken, reason = s"timeout (last used at ${session.touchedAt})")
      true
    } else
      false

  private def delete(token: SessionToken, reason: String): Unit =
    tokenToSession.remove(token) foreach { session ⇒
      logger.info(s"Session #${session.sessionNumber} for User '${session.currentUser.id}' deleted due to $reason")
    }
}

object SessionActor
{
  private val logger = Logger(getClass)

  private[session] def props[S <: Session](newSession: SessionInit[S#User] ⇒ S, config: Config)(implicit s: Scheduler) =
    Props { new SessionActor[S](newSession, config) }

  private[session] sealed trait Command
  private[session] object Command {
    final case class Login[U <: User](user: U, oldSessionTokenOption: Option[SessionToken], isEternalSession: Boolean) extends Command
    final case class Logout(token: SessionToken) extends Command
    final case class Get[U <: User](token: SessionToken, userId: Option[U]) extends Command
    final case object GetCount extends Command
  }

  private case object CleanUp extends DeadLetterSuppression
}
