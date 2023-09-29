package js7.common.akkahttp.web.session

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import akka.util.Timeout
import cats.effect.Resource
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import com.typesafe.config.Config
import izumi.reflect.Tag
import java.nio.file.Files.{createFile, deleteIfExists}
import java.nio.file.Path
import js7.base.Js7Version
import js7.base.auth.{SessionToken, SimpleUser, UserId}
import js7.base.configutils.Configs.*
import js7.base.generic.Completed
import js7.base.io.file.FileUtils.provideFile
import js7.base.io.file.FileUtils.syntax.*
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.JavaTimeConverters.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.version.Version
import js7.common.akkautils.Akkas
import js7.common.http.AkkaHttpClient.`x-js7-session`
import js7.common.system.ServerOperatingSystem.operatingSystem
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Promise

/**
  * @author Joacim Zschimmer
  */
final class SessionRegister[S <: Session: Tag] private[session](
  actor: ActorRef,
  implicit private val akkaAskTimeout: Timeout,
  componentName: String)
extends Service.StoppableByRequest
{
  private val systemSessionPromise = Promise[Checked[S]]()
  val systemSession: Task[Checked[S]] =
    Task.fromFuture(systemSessionPromise.future)
      .logWhenItTakesLonger/*in case it will never been created*/
      .memoize
  val systemUser: Task[Checked[SimpleUser]] =
    systemSession.map(_.map(_.currentUser./*???*/asInstanceOf[SimpleUser]))

  protected def start =
    startService(untilStopRequested)

  def placeSessionTokenInDirectory(user: SimpleUser, workDirectory: Path)
  : Resource[Task, SessionToken] = {
    val sessionTokenFile = workDirectory / "session-token"
    val headersFile = workDirectory / "secret-http-headers"
    provideSessionTokenFile(user, sessionTokenFile)
      .flatTap(sessionToken => provideFile[Task](headersFile)
        .*>(Resource.eval(Task {
          createFile(headersFile, operatingSystem.secretFileAttributes*)
          headersFile := `x-js7-session`.name + ": " + sessionToken.secret.string + "\n"
        })))
  }

  private def provideSessionTokenFile(user: SimpleUser, file: Path): Resource[Task, SessionToken] =
    provideFile[Task](file)
      .flatMap(file => Resource.eval(createSystemSession(user, file)))

  private def createSystemSession(user: SimpleUser, file: Path): Task[SessionToken] =
    for checked <- login(user, Some(Js7Version), isEternalSession = true) yield {
      val sessionToken = checked.orThrow
      deleteIfExists(file)
      createFile(file, operatingSystem.secretFileAttributes*)
      file := sessionToken.secret.string
      //logger.info(s"Session token for internal user '${user.id.string}' placed in file $file")
      systemSessionPromise.completeWith(sessionFuture(sessionToken, Right(user)))
      sessionToken
    }

  def login(
    user: SimpleUser,
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
    catchNonFatalFlatten {
      clientVersion match {
        case None => Checked.unit
        case Some(v) =>
          (v.major == ourVersion.major && v.minor == ourVersion.minor) !!
            Problem.pure(
              s"Client's version $v does not match $componentName version $ourVersion")
      }
    }

  def logout(sessionToken: SessionToken): Task[Completed] =
    Task.deferFuture(
      (actor ? SessionActor.Command.Logout(sessionToken)).mapTo[Completed])

  private[session] def session(sessionToken: SessionToken, idsOrUser: Either[Set[UserId], SimpleUser]): Task[Checked[S]] =
    Task.deferFuture(
      sessionFuture(sessionToken, idsOrUser))

  private[session] def sessionFuture(sessionToken: SessionToken, idsOrUser: Either[Set[UserId], SimpleUser]) =
    (actor ? SessionActor.Command.Get(sessionToken, idsOrUser)).mapTo[Checked[S]]

  @TestOnly
  private[js7] def count: Task[Int] =
    Task.deferFuture(
      (actor ? SessionActor.Command.GetCount).mapTo[Int])

  override def toString = s"SessionRegister[${implicitly[Tag[S]].tag.shortName}]"
}

object SessionRegister
{
  def resource[S <: Session: Tag](
    newSession: SessionInit => S,
    config: Config)
    (implicit arf: ActorRefFactory, scheduler: Scheduler)
  : Resource[Task, SessionRegister[S]] =
    for
      actor <- Akkas.actorResource[Task](
        SessionActor.props(newSession, config),
        implicitly[Tag[S]].tag.longName)
      sessionRegister <- Service.resource(Task(new SessionRegister[S](
        actor,
        akkaAskTimeout = config.getDuration("js7.akka.ask-timeout").toFiniteDuration,
        componentName = config.getString("js7.component.name"))))
    yield sessionRegister

  // Does not stop the Actor !!!
  @TestOnly
  def forTest[S <: Session: Tag]
    (actorRefFactory: ActorRefFactory, newSession: SessionInit => S, config: Config)
    (implicit scheduler: Scheduler)
  : SessionRegister[S] = {
    val sessionActor = actorRefFactory.actorOf(
      SessionActor.props(newSession, config),
      implicitly[Tag[S]].tag.longName)
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
