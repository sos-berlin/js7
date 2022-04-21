package js7.common.akkahttp.web.session

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import akka.util.Timeout
import cats.effect.Resource
import cats.syntax.apply._
import cats.syntax.flatMap._
import com.typesafe.config.Config
import java.nio.file.Files.{createFile, deleteIfExists}
import java.nio.file.Path
import js7.base.Js7Version
import js7.base.auth.{SessionToken, UserId}
import js7.base.configutils.Configs._
import js7.base.generic.Completed
import js7.base.io.file.FileUtils.provideFile
import js7.base.io.file.FileUtils.syntax._
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeConverters._
import js7.base.time.ScalaTime._
import js7.base.utils.Closer
import js7.base.utils.ScalaUtils.syntax._
import js7.base.version.Version
import js7.common.akkahttp.web.session.SessionRegister._
import js7.common.http.AkkaHttpClient.`x-js7-session`
import js7.common.system.ServerOperatingSystem.operatingSystem
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Promise
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class SessionRegister[S <: Session] private[session](
  actor: ActorRef,
  implicit private val akkaAskTimeout: Timeout,
  componentName: String)
{
  private val systemSessionPromise = Promise[Checked[S]]()
  val systemSession: Task[Checked[S]] = Task.fromFuture(systemSessionPromise.future)
  val systemUser: Task[Checked[S#User]] = systemSession.map(_.map(_.currentUser))

  def placeSessionTokenInDirectoryLegacy(user: S#User, workDirectory: Path, closer: Closer)
  : Task[SessionToken] =
    Task.deferAction(implicit s =>
      placeSessionTokenInDirectory(user, workDirectory)
        .allocated
        .logWhenItTakesLonger("createSystemSession")
        .flatMap { case (sessionToken, release) =>
          Task {
            closer.onClose {
              try release.runSyncUnsafe(5.s)
              catch { case NonFatal(t) => logger.error(
                s"createSessionTokenFile release => ${t.toStringWithCauses}")
              }
            }
            sessionToken
          }
        })

  def placeSessionTokenInDirectory(user: S#User, workDirectory: Path)
  : Resource[Task, SessionToken] = {
    val sessionTokenFile = workDirectory / "session-token"
    val headersFile = workDirectory / "secret-http-headers"
    provideSessionTokenFile(user, sessionTokenFile)
      .flatTap(sessionToken => provideFile[Task](headersFile)
        .*>(Resource.eval(Task {
          createFile(headersFile, operatingSystem.secretFileAttributes: _*)
          headersFile := `x-js7-session`.name + ": " + sessionToken.secret.string + "\n"
        })))
  }

  private def provideSessionTokenFile(user: S#User, file: Path): Resource[Task, SessionToken] =
    provideFile[Task](file)
      .flatMap(file => Resource.eval(createSystemSession(user, file)))

  private def createSystemSession(user: S#User, file: Path): Task[SessionToken] =
    for (checked <- login(user, Some(Js7Version), isEternalSession = true)) yield {
      val sessionToken = checked.orThrow
      deleteIfExists(file)
      createFile(file, operatingSystem.secretFileAttributes: _*)
      file := sessionToken.secret.string
      //logger.info(s"Session token for internal user '${user.id.string}' placed in file $file")
      systemSessionPromise.completeWith(sessionFuture(sessionToken, Right(user)))
      sessionToken
    }

  def login(
    user: S#User,
    clientVersion: Option[Version],
    sessionTokenOption: Option[SessionToken] = None,
    isEternalSession: Boolean = false)
  : Task[Checked[SessionToken]] =
    Task
      .deferFuture(
        (actor ? SessionActor.Command.Login(user, clientVersion, sessionTokenOption,
          isEternalSession = isEternalSession)).mapTo[SessionToken])
      .map(Right(_))

  private[session] def checkNonMatchingVersion(
    clientVersion: Option[Version],
    ourVersion: Version = Js7Version)
  : Checked[Unit] =
    Checked.catchNonFatal {
      clientVersion match {
        case None => Checked.unit
        case Some(v) =>
          (v.major == ourVersion.major && v.minor == ourVersion.minor) !!
            Problem.pure(
              s"Client's version $v does not match $componentName version $ourVersion")
      }
    }.flatten

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
  private val logger = Logger[this.type]

  def start[S <: Session](actorRefFactory: ActorRefFactory, newSession: SessionInit[S#User] => S, config: Config)
    (implicit scheduler: Scheduler)
  : SessionRegister[S] = {
    val sessionActor = actorRefFactory.actorOf(SessionActor.props[S](newSession, config), "session")
    new SessionRegister[S](sessionActor,
      akkaAskTimeout = config.getDuration("js7.akka.ask-timeout").toFiniteDuration,
      componentName = config.getString("js7.component.name"))
  }

  val TestConfig: Config = config"""
    js7.component.name = "JS7 TEST"
    js7.akka.ask-timeout = 99s
    js7.auth.session.timeout = 1 minute
    """
}
