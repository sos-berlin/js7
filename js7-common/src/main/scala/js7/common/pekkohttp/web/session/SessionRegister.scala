package js7.common.pekkohttp.web.session

import cats.effect.std.{AtomicCell, Supervisor}
import cats.effect.{Deferred, FiberIO, IO, Resource, ResourceIO}
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.option.*
import com.typesafe.config.Config
import izumi.reflect.Tag
import java.nio.file.Files.{createFile, deleteIfExists}
import java.nio.file.Path
import js7.base.Js7Version
import js7.base.auth.{SessionToken, SimpleUser, UserId}
import js7.base.catsutils.CatsDeadline
import js7.base.catsutils.Environment.environment
import js7.base.catsutils.UnsafeMemoizable.{memoize, unsafeMemoize}
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.provideFile
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.system.ServerOperatingSystem.operatingSystem
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.WallClock
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.logWhenMethodTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.version.{Js7Versions, Version}
import js7.common.auth.SecretStringGenerator
import js7.common.http.PekkoHttpClient.`x-js7-session`
import js7.common.pekkohttp.web.session.SessionRegister.*
import org.jetbrains.annotations.TestOnly
import scala.collection.View
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class SessionRegister[S <: Session: Tag] private(
  newSession: SessionInit => S,
  config: Config,
  supervisor: Supervisor[IO])
extends Service.TrivialReleasable:

  private val componentName = config.getString("js7.component.name")
  private val sessionLifetime = config.getDuration("js7.auth.session.timeout").toFiniteDuration

  private val cell = AtomicCell[IO].of(State(Map.empty)).unsafeMemoize
  private val deferredSystemSession = Deferred.unsafe[IO, Checked[S]]

  val systemSession: IO[Checked[S]] =
    memoize:
      deferredSystemSession.get
        .logWhenMethodTakesLonger/*in case it will never been created*/

  val systemUser: IO[Checked[SimpleUser]] =
    systemSession.map(_.map(_.currentUser))

  protected def release =
    cell.flatMap(_.get).flatMap: state =>
      IO(state.logOpenSessions())

  def placeSessionTokenInDirectory(user: SimpleUser, workDirectory: Path)
  : ResourceIO[SessionToken] =
    val sessionTokenFile = workDirectory / "session-token"
    val headersFile = workDirectory / "secret-http-headers"
    provideSessionTokenFile(user, sessionTokenFile).flatTap: sessionToken =>
      provideFile[IO](headersFile) *>
        Resource.eval:
          IO.blocking:
            createFile(headersFile, operatingSystem.secretFileAttributes *)
            headersFile := `x-js7-session`.name + ": " + sessionToken.secret.string + "\n"

  private def provideSessionTokenFile(user: SimpleUser, file: Path): ResourceIO[SessionToken] =
    provideFile[IO](file).flatMap: file =>
      Resource.eval(createSystemSession(user, file))

  private def createSystemSession(user: SimpleUser, file: Path): IO[SessionToken] =
    for
      token <- login(user, source = "System", Some(Js7Version), isEternalSession = true)
      _ <- IO.interruptible:
        deleteIfExists(file)
        createFile(file, operatingSystem.secretFileAttributes*)
        file := token.secret.string
        logger.debug(s"SessionToken for internal ${user.id} has been placed in file $file")
      checkedSession <- session(token, Right(user))
      _ <- deferredSystemSession.complete(checkedSession)
    yield
      token

  def login(
    user: SimpleUser,
    source: String,
    clientVersion: Option[Version],
    token: Option[SessionToken] = None,
    isEternalSession: Boolean = false)
  : IO[SessionToken] =
    cell.flatMap(_.evalModify: state =>
      token.fold(IO.pure(state)): token =>
        state.delete(token, "deleted due to Login")
      .flatMap: state =>
        environment[WallClock].flatMap: clock =>
          IO:
            val now = clock.now()
            val token = SessionToken.generateFromSecretString:
              SecretStringGenerator.newSecretString()
            assertThat( // paranoid
              !state.tokenToSession.contains(token), "Duplicate generated SessionToken")
            val session = newSession:
              SessionInit(token, user, source, now,
                until = !isEternalSession ? (Deadline.now + sessionLifetime))
            session.lastUsed = Session.LastUsed("login", now)
            val entry = Entry(session)

            val updated = state.copy(
              tokenToSession = state.tokenToSession.updated(session.sessionToken, entry))

            logger.info(s"${session.sessionToken} for ${user.id} Login" +
              clientVersion.fold("")(v =>
                " (" + v + (
                  if v == Js7Version then
                    " ✔)"
                  else
                    s" ⚠️ version differs from this server's version $Js7Version!)")) +
              (isEternalSession ?? " (eternal)"))

            clientVersion match
              case None => logger.warn("Client does not provide its version")
              case Some(v) =>
                Js7Versions.checkNonMatchingVersion(v, otherName = user.id.toString)
                  .left.foreach: problem =>
                    logger.error(problem.toString)

            updated -> entry)
    .flatMap: entry =>
      Bean.sessionCount += 1
      IO.unlessA(isEternalSession):
        CatsDeadline.now.flatMap: now =>
          val timeoutAt = now + sessionLifetime
          supervisor.supervise:
            cell.flatMap(_.update:
              _.deleteOnly(entry.session.sessionToken, "expired"))
            .delayBy(sessionLifetime)
          .map: fiber =>
            entry.timeoutFiber = Some(fiber)
      .as(entry.session.sessionToken)

  def logout(sessionToken: SessionToken): IO[Unit] =
    cell.flatMap(_.evalUpdate:
      _.delete(sessionToken, "Logout"))

  private[session] def session(
    token: SessionToken,
    idsOrUser: Either[Set[UserId], SimpleUser],
    usedFor: String = "")
  : IO[Checked[S]] =
    environment[WallClock].flatMap: clock =>
      cell.flatMap(_.get).map: state =>
        (state.tokenToSession.get(token), idsOrUser) match
          case (None, _) =>
            val users = idsOrUser.fold(_.mkString("|"), _.id)
            logger.debug(s"🔒 InvalidSessionToken: Rejecting unknown $token of $users")
            Left(InvalidSessionTokenProblem)

          case (Some(entry), Right(user))
            if !user.id.isAnonymous && user.id != entry.currentUserId =>
            tryUpdateLatelyAuthenticatedUser(user, entry.session)

          case (Some(entry), Left(userIds)) if !userIds.contains(entry.currentUserId) =>
            logger.debug("🔒 InvalidSessionToken: HTTPS distinguished name UserIds " +
              s"'${userIds.mkString(", ")}' do not include Sessions's ${entry.currentUserId}")
            Left(InvalidSessionTokenProblem)

          case (Some(entry), _) =>
            entry.session.lastUsed = Session.LastUsed(usedFor, clock.now())
            Right(entry.session)
    .flatTapT: session =>
      IO(session.isAlive !! InvalidSessionTokenProblem)

  def sessions: IO[Seq[S]] =
    cell.flatMap(_.get).map:
      _.tokenToSession.values.toVector.map(_.session)

  @TestOnly
  private[js7] def count: IO[Int] =
    cell.flatMap(_.get).map(_.
      tokenToSession.size)

  /** Handles late authentication for Browser usage.
   * login(None) was accepted without authentication, establishing a Session for Anonymous.
   * A web service calling authorize() may require more rights than Anonymous,
   * leading to 401 and browser authentication dialog.
   * The resent (current) request has a HTTP authentication header, which was authentiated (see caller)
   * The session is updated with the authenticated user.
   * This may happen only once and the original user must be Anonymous.
   */
  private def tryUpdateLatelyAuthenticatedUser(newUser: SimpleUser, session: S): Checked[S] =
    if session.sessionInit.loginUser.isAnonymous && session.tryUpdateUser(newUser) then
      logger.info:
        s"${session.sessionToken} for ${session.sessionInit.loginUser.id} switched to ${newUser.id}"
      Right(session)
    else
      logger.debug(s"🔒 InvalidSessionToken: ${session.sessionToken}: Rejecting session token " +
        s"belonging to ${session.currentUser.id} but sent by ${newUser.id}")
      Left(InvalidSessionTokenProblem)


  private[session] def checkNonMatchingVersion(
    clientVersion: Option[Version],
    ourVersion: Version = Js7Version)
  : Checked[Unit] =
    catchNonFatalFlatten:
      clientVersion match
        case None => Checked.unit
        case Some(v) =>
          (v.major == ourVersion.major && v.minor == ourVersion.minor) !!
            Problem.pure(
              s"Client's version $v does not match $componentName version $ourVersion")

  override def toString =
    s"SessionRegister[${implicitly[Tag[S]].tag.shortName}]"


  private final case class State(tokenToSession: Map[SessionToken, Entry]):
    /** Cancel the timeout fiber, too. */
    def delete(token: SessionToken, reason: String): IO[State] =
      tokenToSession.get(token).fold(IO.pure(this)): session =>
        session.timeoutFiber.foldMap(_.cancel).as:
          deleteOnly(token, "expired")

    def deleteOnly(token: SessionToken, reason: String): State =
      tokenToSession.get(token).fold(this): entry =>
        logger.info(s"$token for ${entry.currentUserId} $reason")
        entry.session.die()
        Bean.sessionCount -= 1
        copy(tokenToSession = tokenToSession.removed(token))

    def logOpenSessions(): Unit =
      sessions
        .groupMap(_.currentUser.id)(_.sessionToken)
        .view.mapValues(_.toVector.sortBy(_.number))
        .toVector.sortBy(_._1)
        .foreach: (userId, sessionTokens) =>
          logger.debug(s"${sessionTokens.size} open sessions for $userId${
            sessionTokens.view.mkString(" (", " ", ")")}")

    def sessions: View[Session] =
      tokenToSession.values.view.map(_.session)

  private final class Entry(val session: S):
    def currentUserId = session.currentUser.id
    var timeoutFiber = none[FiberIO[Unit]]
    override def toString = currentUserId.toString


object SessionRegister:

  private val logger = Logger[this.type]

  def service[S <: Session: Tag](newSession: SessionInit => S, config: Config)
  : ResourceIO[SessionRegister[S]] =
    for
      supervisor <- Supervisor[IO]
      service <- Service(new SessionRegister[S](newSession, config, supervisor))
    yield
      service

  @TestOnly
  val TestTimeout: FiniteDuration = 1.minute

  @TestOnly
  val TestConfig: Config = config"""
    js7.component.name = "JS7 TEST"
    js7.auth.session.timeout = ${TestTimeout.toMillis} milliseconds
    """

  sealed trait SessionsMXBean:
    this: Bean.type =>

    def getSessionCount: Int =
      sessionCount.get

  object Bean extends SessionsMXBean:
    val sessionCount = Atomic(0)
