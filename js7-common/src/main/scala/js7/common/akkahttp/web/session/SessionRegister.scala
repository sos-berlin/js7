package js7.common.akkahttp.web.session

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.Config
import java.nio.file.Files.{createFile, deleteIfExists}
import java.nio.file.Path
import js7.base.auth.{SessionToken, UserId}
import js7.base.configutils.Configs._
import js7.base.generic.Completed
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked
import js7.base.time.JavaTimeConverters._
import js7.common.system.ServerOperatingSystem.operatingSystem
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Promise

/**
  * @author Joacim Zschimmer
  */
final class SessionRegister[S <: Session] private[session](actor: ActorRef, implicit private val akkaAskTimeout: Timeout)
{
  private val systemSessionPromise = Promise[Checked[S]]()
  val systemSession: Task[Checked[S]] = Task.fromFuture(systemSessionPromise.future)
  val systemUser: Task[Checked[S#User]] = systemSession.map(_.map(_.currentUser))

  def createSystemSession(user: S#User, file: Path): Task[SessionToken] =
    for (sessionToken <- login(user, clientVersion = None, isEternalSession = true)) yield {
      deleteIfExists(file)
      createFile(file, operatingSystem.secretFileAttributes: _*)
      file := sessionToken.secret.string
      //logger.info(s"Session token for internal user '${user.id.string}' placed in file $file")
      systemSessionPromise.completeWith(sessionFuture(sessionToken, Right(user)))
      sessionToken
    }

  def login(
    user: S#User,
    clientVersion: Option[String],
    sessionTokenOption: Option[SessionToken] = None,
    isEternalSession: Boolean = false
  ): Task[SessionToken] =
    Task.deferFuture(
      (actor ? SessionActor.Command.Login(user, clientVersion, sessionTokenOption,
        isEternalSession = isEternalSession)).mapTo[SessionToken])

  def logout(sessionToken: SessionToken): Task[Completed] =
    Task.deferFuture(
      (actor ? SessionActor.Command.Logout(sessionToken)).mapTo[Completed])

  private[session] def session(sessionToken: SessionToken, idsOrUser: Either[Set[UserId], S#User]): Task[Checked[S]] =
    Task.deferFuture(
      sessionFuture(sessionToken, idsOrUser))

  private[session] def sessionFuture(sessionToken: SessionToken, idsOrUser: Either[Set[UserId], S#User]) =
    (actor ? SessionActor.Command.Get(sessionToken, idsOrUser)).mapTo[Checked[S]]

  @TestOnly
  private[js7] def count: Task[Int] =
    Task.deferFuture(
      (actor ? SessionActor.Command.GetCount).mapTo[Int])
}

object SessionRegister
{
  def start[S <: Session](actorRefFactory: ActorRefFactory, newSession: SessionInit[S#User] => S, config: Config)
    (implicit scheduler: Scheduler)
  : SessionRegister[S] = {
    val sessionActor = actorRefFactory.actorOf(SessionActor.props[S](newSession, config), "session")
    new SessionRegister[S](sessionActor,
      akkaAskTimeout = config.getDuration("js7.akka.ask-timeout").toFiniteDuration)
  }

  val TestConfig: Config = config"""
    js7.akka.ask-timeout = 99.s
    js7.auth.session.timeout = 1 minute
    """
}
