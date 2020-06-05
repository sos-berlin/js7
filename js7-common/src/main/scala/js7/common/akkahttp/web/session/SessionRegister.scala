package js7.common.akkahttp.web.session

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Files.{createFile, deleteIfExists}
import java.nio.file.Path
import js7.base.auth.SessionToken
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.common.akkahttp.web.session.SessionRegister._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Logger
import js7.common.system.OperatingSystem.operatingSystem
import js7.common.time.JavaTimeConverters._
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
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
      deleteIfExists(file)
      createFile(file, operatingSystem.secretFileAttributes: _*)
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

  @TestOnly
  private[js7] def count: Task[Int] =
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
      akkaAskTimeout = config.getDuration("js7.akka.ask-timeout").toFiniteDuration)
  }

  val TestConfig: Config = ConfigFactory.parseString(
    """js7.akka.ask-timeout = 99.s
      |js7.auth.session.timeout = 1 minute
      |""".stripMargin)
}
