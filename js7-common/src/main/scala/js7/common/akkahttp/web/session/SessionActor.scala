package js7.common.akkahttp.web.session

import akka.actor.{Actor, DeadLetterSuppression, Props}
import com.typesafe.config.Config
import js7.base.auth.{SessionToken, User, UserId}
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.problem.Checked._
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.time.JavaTimeConverters._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.implicits.InsertableMutableMap
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkahttp.web.session.SessionActor._
import js7.common.auth.SecretStringGenerator
import monix.execution.{Cancelable, Scheduler}
import scala.collection.mutable

// TODO https://www.owasp.org/index.php/Session_Management_Cheat_Sheet
/**
 * Executes public Agent commands.
 *
 * @author Joacim Zschimmer
 */
final class SessionActor[S <: Session] private(newSession: SessionInit[S#User] => S, config: Config)
  (implicit scheduler: Scheduler)
extends Actor {

  private val sessionTimeout = config.getDuration("js7.auth.session.timeout").toFiniteDuration
  private val cleanupInterval = sessionTimeout / 4
  private val tokenToSession = mutable.Map.empty[SessionToken, S]
  private var nextCleanup: Cancelable = null

  override def postStop(): Unit = {
    val openSessionsString = tokenToSession.values
      .groupBy(_.currentUser.id)
      .view.mapValues(_.toVector.sortBy(_.sessionNumber))
      .toVector.sortBy(_._1)
      .view
      .map { case (u, s) => s.map(_.sessionToken.short).mkString(" ") + ":" + u.string }
      .mkString(" ")
    logger.debug(s"postStop: ${tokenToSession.size} open sessions: $openSessionsString")
    if (nextCleanup != null) nextCleanup.cancel()
    super.postStop()
  }

  def receive = {
    case Command.Login(_user: User, tokenOption, isEternalSession) =>
      val user = _user.asInstanceOf[S#User]
      for (t <- tokenOption) delete(t, reason = "second login")
      val token = SessionToken.generateFromSecretString(SecretStringGenerator.newSecretString())
      assertThat(!tokenToSession.contains(token), s"Duplicate generated SessionToken")  // Must not happen
      val session = newSession(SessionInit(token, user))
      if (!isEternalSession) {
        session.touch(sessionTimeout)
      }
      tokenToSession.insert(session.sessionToken -> session)
      logger.info(s"Session:${session.sessionNumber} for User '${user.id}' added${session.isEternal ?? " (eternal)"}")
      sender() ! token
      scheduleNextCleanup()

    case Command.Logout(token) =>
      delete(token, reason = "logout")
      sender() ! Completed

    case Command.Get(token, _idsOrUser: Either[Set[UserId], User]) =>
      val idsOrUser = _idsOrUser.map(_.asInstanceOf[S#User])
      val sessionOption = (tokenToSession.get(token), idsOrUser) match {
        case (None, _) =>
          logger.debug(s"Rejecting unknown session token of user '${idsOrUser.fold(identity, _.id)}'")
          None

        case (Some(session), Right(user)) if !user.id.isAnonymous && user.id != session.currentUser.id =>
          tryUpdateLatelyAuthenticatedUser(user, session)

        case (Some(session), Left(userIds)) if !userIds.contains(session.currentUser.id) =>
          logger.debug(s"HTTPS distinguished name UserIds '${userIds.mkString(", ")}'" +
            s" do not include Sessions's User '${session.currentUser.id}''")
          None

        case (Some(session), _) =>
          if (handleTimeout(session)) {
            None
          } else {
            if (!session.isEternal) {
              session.touch(sessionTimeout)
            }
            Some(session)
          }
      }
      val checkedSession = sessionOption toChecked InvalidSessionTokenProblem
      sender() ! checkedSession

    case Command.GetCount =>
      sender() ! tokenToSession.size

    case CleanUp =>
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
    if (session.sessionInit.loginUser.isAnonymous &&
        session.tryUpdateUser(newUser.asInstanceOf[session.User]))  // Mutate session!
    {
      logger.info(s"Session:${session.sessionNumber} for User '${session.sessionInit.loginUser.id}' switched to User '${newUser.id}'")
      Some(session)
    } else {
      logger.debug(s"Rejecting session token ${session.sessionNumber} belonging to user '${session.currentUser.id}' but sent by user '${newUser.id.string}'")
      None
    }
  }

  private def scheduleNextCleanup(): Unit =
    if (nextCleanup == null && tokenToSession.values.exists(o => !o.isEternal)) {
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
    tokenToSession.remove(token) foreach { session =>
      logger.info(s"Session:${session.sessionNumber} for User '${session.currentUser.id}' deleted due to $reason")
    }
}

object SessionActor
{
  private val logger = Logger(getClass)

  private[session] def props[S <: Session](newSession: SessionInit[S#User] => S, config: Config)(implicit s: Scheduler) =
    Props { new SessionActor[S](newSession, config) }

  private[session] sealed trait Command
  private[session] object Command {
    final case class Login[U <: User](user: U, oldSessionTokenOption: Option[SessionToken], isEternalSession: Boolean) extends Command
    final case class Logout(token: SessionToken) extends Command
    final case class Get[U <: User](token: SessionToken, idsOrUser: Either[Set[UserId], U]) extends Command
    final case object GetCount extends Command
  }

  private case object CleanUp extends DeadLetterSuppression
}
