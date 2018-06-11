package com.sos.jobscheduler.common.akkahttp.web.session

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.base.auth.{SessionToken, UserId}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRegister._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.{RichPath, pathToFile}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.OperatingSystem.operatingSystem
import java.nio.file.{Files, Path}
import monix.eval.Task
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
final class SessionRegister[S <: LoginSession] private[session](actor: ActorRef, implicit private val akkaAskTimeout: Timeout)
{
  private val systemSessionPromise = Promise[Checked[S]]()
  val systemSession: Task[Checked[S]] = Task.fromFuture(systemSessionPromise.future)

  def createSystemSession(user: S#User, file: Path): Task[Completed] =
    for (sessionToken ← login(user)/*TODO No session timeout*/) yield {
      file.delete()
      Files.createFile(file, operatingSystem.secretFileAttributes: _*)
      file.contentString = sessionToken.secret.string
      logger.info(s"Session for user '${user.id.string}' token placed in file $file")
      systemSessionPromise.completeWith(sessionFuture(Some(user.id), sessionToken))
      Completed
    }

  def login(user: S#User, sessionTokenOption: Option[SessionToken] = None): Task[SessionToken] =
    Task.deferFuture(
      (actor ? SessionActor.Command.Login(user, sessionTokenOption)).mapTo[SessionToken])

  def logout(sessionToken: SessionToken): Task[Completed] =
    Task.deferFuture(
      (actor ? SessionActor.Command.Logout(sessionToken)).mapTo[Completed])

  private[session] def session(sessionToken: SessionToken, userId: Option[UserId]): Task[Checked[S]] =
    Task.deferFuture(
      sessionFuture(userId, sessionToken))

  private[session] def sessionFuture(userId: Option[UserId], sessionToken: SessionToken): Future[Checked[S]] =
    (actor ? SessionActor.Command.Get(sessionToken, userId)).mapTo[Checked[S]]
}

object SessionRegister
{
  private val logger = Logger(getClass)

  def start[S <: LoginSession](
    actorRefFactory: ActorRefFactory,
    newSession: SessionInit[S#User] ⇒ S,
    akkaAskTimeout: Timeout)
  : SessionRegister[S] = {
    val sessionActor = actorRefFactory.actorOf(SessionActor.props[S](newSession), "session")
    new SessionRegister[S](sessionActor, akkaAskTimeout = akkaAskTimeout)
  }
}
