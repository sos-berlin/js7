package js7.common.akkahttp.web.session

import akka.actor.{Actor, DeadLetterSuppression, Props}
import com.typesafe.config.Config
import js7.base.Js7Version
import js7.base.auth.{SessionToken, SimpleUser, User, UserId}
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.time.JavaTimeConverters.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.implicits.InsertableMutableMap
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.version.{Js7Versions, Version}
import js7.common.akkahttp.web.session.SessionActor.*
import js7.common.auth.SecretStringGenerator
import monix.execution.{Cancelable, Scheduler}
import scala.collection.mutable

// TODO https://www.owasp.org/index.php/Session_Management_Cheat_Sheet
/**
 * Executes public Agent commands.
 *
 * @author Joacim Zschimmer
 */
final class SessionActor[S <: Session] private
  (newSession: SessionInit => S, config: Config)
  (implicit scheduler: Scheduler)
extends Actor {

  private val sessionTimeout = config.getDuration("js7.auth.session.timeout").toFiniteDuration
  private val cleanupInterval = sessionTimeout / 4
  private val tokenToSession = mutable.Map.empty[SessionToken, S]
  private var nextCleanup: Cancelable = null

  override def postStop() = {
    if (nextCleanup != null) nextCleanup.cancel()
    tokenToSession.values
      .groupMap(_.currentUser.id)(_.sessionToken)
      .view.mapValues(_.toVector.sortBy(_.number))
      .toVector.sortBy(_._1)
      .foreach { case (userId, sessionTokens) =>
        logger.debug(sessionTokens.size.toString + " open sessions for " + userId +
          sessionTokens.view.mkString(" (", " ", ")"))
      }
    super.postStop()
  }

  def receive = {
    case Command.Login(_user: User, clientVersion, tokenOption, isEternalSession) =>
      val user = _user.asInstanceOf[SimpleUser]
      for (t <- tokenOption) delete(t, reason = "second login")
      val token = SessionToken.generateFromSecretString(SecretStringGenerator.newSecretString())
      assertThat(!tokenToSession.contains(token), s"Duplicate generated SessionToken")  // Must not happen
      val session = newSession(SessionInit(token, user))
      if (!isEternalSession) {
        session.touch(sessionTimeout)
      }
      tokenToSession.insert(session.sessionToken, session)

      logger.info(s"${session.sessionToken} for ${user.id}: Login" +
        clientVersion.fold("")(v =>
          " (" + v + (
            if (v == Js7Version)
              " ✔)"
            else
              s" ⚠️ version differs from this server's version $Js7Version!)")) +
        (session.isEternal ?? " (eternal)"))

      clientVersion match {
        case None => logger.warn("Client does not provide its version")
        case Some(v) =>
          Js7Versions.checkNonMatchingVersion(v, otherName = user.id.toString)
            .left
            .foreach { problem =>
              logger.error(problem.toString)
            }
      }

      sender() ! token
      scheduleNextCleanup()

    case Command.Logout(token) =>
      for (session <- tokenToSession.remove(token)) {
        logger.info(s"$token for ${session.currentUser.id}: Logout")
      }
      sender() ! Completed

    case Command.Get(token, idsOrUser: Either[Set[UserId @unchecked], SimpleUser]) =>
      val checkedSession = (tokenToSession.get(token), idsOrUser) match {
        case (None, _) =>
          val users = idsOrUser.fold(_.mkString("|"), _.id)
          logger.debug(s"⛔️ InvalidSessionToken: Rejecting unknown session token of $users")
          Left(InvalidSessionTokenProblem)

        case (Some(session), Right(user)) if !user.id.isAnonymous && user.id != session.currentUser.id =>
          tryUpdateLatelyAuthenticatedUser(user, session)

        case (Some(session), Left(userIds)) if !userIds.contains(session.currentUser.id) =>
          logger.debug("⛔️ InvalidSessionToken: HTTPS distinguished name UserIds " +
            s"'${userIds.mkString(", ")}' do not include Sessions's ${session.currentUser.id}")
          Left(InvalidSessionTokenProblem)

        case (Some(session), _) =>
          if (handleTimeout(session)) {
            Left(InvalidSessionTokenProblem)
          } else {
            if (!session.isEternal) {
              session.touch(sessionTimeout)
            }
            Right(session)
          }
      }
      sender() ! (checkedSession: Checked[Session])

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
  private def tryUpdateLatelyAuthenticatedUser(newUser: SimpleUser, session: Session): Checked[Session] = {
    if (session.sessionInit.loginUser.isAnonymous &&
        session.tryUpdateUser(newUser))  // Mutate session!
    {
      logger.info(s"${session.sessionToken} for ${session.sessionInit.loginUser.id} switched to ${newUser.id}")
      Right(session)
    } else {
      logger.debug(s"⛔️ InvalidSessionToken: ${session.sessionToken}: Rejecting session token " +
        s"belonging to ${session.currentUser.id} but sent by ${newUser.id}")
      Left(InvalidSessionTokenProblem)
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
      logger.info(s"$token for ${session.currentUser.id} deleted due to $reason")
    }
}

object SessionActor
{
  private val logger = Logger(getClass)

  private[session] def props[S <: Session]
    (newSession: SessionInit => S, config: Config)
    (implicit s: Scheduler) =
    Props { new SessionActor(newSession, config) }

  private[session] sealed trait Command
  private[session] object Command {
    final case class Login[U <: User](
      user: U,
      js7Version: Option[Version],
      oldSessionTokenOption: Option[SessionToken],
      isEternalSession: Boolean
    ) extends Command
    final case class Logout(token: SessionToken) extends Command
    final case class Get[U <: User](token: SessionToken, idsOrUser: Either[Set[UserId], U]) extends Command
    case object GetCount extends Command
  }

  private case object CleanUp extends DeadLetterSuppression
}
