package com.sos.jobscheduler.common.akkahttp.web.session

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.base.auth.SessionToken
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.akkahttp.web.session.SessionRegister._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.{RichPath, pathToFile}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.OperatingSystem.operatingSystem
import com.sos.jobscheduler.common.time.ScalaTime._
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.{Files, Path}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
final class SessionRegister[S <: Session] private[session](actor: ActorRef, implicit private val akkaAskTimeout: Timeout)
{
  private val systemSessionPromise = Promise[Checked[S]]()
  val systemSession: Task[Checked[S]] = Task.fromFuture(systemSessionPromise.future)

  def createSystemSession(user: S#User, file: Path): Task[SessionToken] =
    for (sessionToken <- login(user, isEternalSession = true)) yield {
      file.delete()
      Files.createFile(file, operatingSystem.secretFileAttributes: _*)
      file := sessionToken.secret.string
      logger.info(s"Session token for internal user '${user.id.string}' placed in file $file")
      systemSessionPromise.completeWith(sessionFuture(Some(user), sessionToken))
      sessionToken
    }

  def login(user: S#User, sessionTokenOption: Option[SessionToken] = None, isEternalSession: Boolean = false): Task[SessionToken] =
    Task.deferFuture(
      (actor ? SessionActor.Command.Login(user, sessionTokenOption, isEternalSession = isEternalSession)).mapTo[SessionToken])

  def logout(sessionToken: SessionToken): Task[Completed] =
    Task.deferFuture(
      (actor ? SessionActor.Command.Logout(sessionToken)).mapTo[Completed])

  private[session] def session(sessionToken: SessionToken, user: Option[S#User]): Task[Checked[S]] =
    Task.deferFuture(
      sessionFuture(user, sessionToken))

  private[session] def sessionFuture(user: Option[Session#User], sessionToken: SessionToken): Future[Checked[S]] =
    (actor ? SessionActor.Command.Get(sessionToken, user)).mapTo[Checked[S]]

  private[session] def count: Task[Int] =
    Task.deferFuture(
      (actor ? SessionActor.Command.GetCount).mapTo[Int])
}

object SessionRegister
{
  private val logger = Logger(getClass)

  def start[S <: Session](actorRefFactory: ActorRefFactory, newSession: SessionInit[S#User] => S, config: Config)
    (implicit scheduler: Scheduler)
  : SessionRegister[S] = {
    val sessionActor = actorRefFactory.actorOf(SessionActor.props[S](newSession, config), "session")
    new SessionRegister[S](sessionActor,
      akkaAskTimeout = config.getDuration("jobscheduler.akka-ask-timeout").toFiniteDuration)
  }

  val TestConfig: Config = ConfigFactory.parseString(
    """jobscheduler.akka-ask-timeout = 99.s
      |jobscheduler.auth.session.timeout = 1 minute
      |""".stripMargin)
}
