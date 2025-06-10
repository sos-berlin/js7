package js7.common.pekkohttp.web.session

import cats.effect.std.AtomicCell
import cats.effect.{Deferred, FiberIO, IO, Resource, ResourceIO}
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import izumi.reflect.Tag
import java.nio.file.Files.{createFile, deleteIfExists}
import java.nio.file.Path
import js7.base.Js7Version
import js7.base.auth.{SessionToken, SimpleUser, UserId}
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.UnsafeMemoizable.{memoize, unsafeMemoize}
import js7.base.configutils.Configs.*
import js7.base.generic.Completed
import js7.base.io.file.FileUtils.provideFile
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.system.ServerOperatingSystem.operatingSystem
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Atomic
import js7.base.utils.CatsUtils.syntax.logWhenMethodTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.version.{Js7Versions, Version}
import js7.common.auth.SecretStringGenerator
import js7.common.http.PekkoHttpClient.`x-js7-session`
import js7.common.pekkohttp.web.session.SessionRegister.*
import org.jetbrains.annotations.TestOnly
import scala.util.chaining.scalaUtilChainingOps

/**
  * @author Joacim Zschimmer
  */
final class SessionRegister[S <: Session: Tag] private[session](
  newSession: SessionInit => S,
  config: Config)
extends Service.StoppableByRequest:

  private val componentName = config.getString("js7.component.name")
  private val sessionTimeout = config.getDuration("js7.auth.session.timeout").toFiniteDuration
  private val cleanupInterval = sessionTimeout / 4

  private val cell = AtomicCell[IO].of(State(Map.empty)).unsafeMemoize

  private val deferredSystemSession = Deferred.unsafe[IO, Checked[S]]

  val systemSession: IO[Checked[S]] =
    memoize:
      deferredSystemSession.get
        .logWhenMethodTakesLonger/*in case it will never been created*/

  val systemUser: IO[Checked[SimpleUser]] =
    systemSession.map(_.map(_.currentUser))

  protected def start =
    startService(untilStopRequested *> onStop)

  private def onStop =
    //??? if scheduledCleanUp != null then scheduledCleanUp.cancel()
    cell.flatMap(_.get).flatMap(state => IO:
      state.logOpenSessions())

  def placeSessionTokenInDirectory(user: SimpleUser, workDirectory: Path)
  : ResourceIO[SessionToken] =
    val sessionTokenFile = workDirectory / "session-token"
    val headersFile = workDirectory / "secret-http-headers"
    provideSessionTokenFile(user, sessionTokenFile)
      .flatTap(sessionToken => provideFile[IO](headersFile)
        .*>(Resource.eval(IO {
          createFile(headersFile, operatingSystem.secretFileAttributes*)
          headersFile := `x-js7-session`.name + ": " + sessionToken.secret.string + "\n"
        })))

  private def provideSessionTokenFile(user: SimpleUser, file: Path): ResourceIO[SessionToken] =
    provideFile[IO](file)
      .flatMap(file => Resource.eval(createSystemSession(user, file)))

  private def createSystemSession(user: SimpleUser, file: Path): IO[SessionToken] =
    for
      token <- login(user, Some(Js7Version), isEternalSession = true)
      _ <- IO.interruptible:
        deleteIfExists(file)
        createFile(file, operatingSystem.secretFileAttributes*)
        file := token.secret.string
        logger.debug(s"SessionToken for internal ${user.id} has been placed in file $file")
      checkedSession <- session(token, Right(user))
      _ <- deferredSystemSession.complete(checkedSession)
    yield token

  def login(
    user: SimpleUser,
    clientVersion: Option[Version],
    token: Option[SessionToken] = None,
    isEternalSession: Boolean = false)
  : IO[SessionToken] =
    cell
      .flatMap(_.modify: state =>
        token
          .fold(state)(oldToken => state
            .delete(oldToken, " deleted due to Login"))
          .pipe: state =>
            val token = SessionToken.generateFromSecretString(
              SecretStringGenerator.newSecretString())
            assertThat( // paranoid
              !state.tokenToSession.contains(token), "Duplicate generated SessionToken")
            val session = newSession(SessionInit(token, user))

            val updated = state.copy(
              tokenToSession = state.tokenToSession.updated(session.sessionToken, session))

            logger.info(s"${session.sessionToken} for ${user.id}: Login" +
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
                  .left
                  .foreach: problem =>
                    logger.error(problem.toString)

            updated -> session)
       .flatMap: session =>
         IO
           .unlessA(isEternalSession):
             session.touch(sessionTimeout) *> cleanUp.scheduleNext
           .as(session.sessionToken)

  def logout(sessionToken: SessionToken): IO[Completed] =
    cell
      .flatMap(_.update(_
        .delete(sessionToken, ": Logout")))
      .as(Completed)

  private[session] def session(token: SessionToken, idsOrUser: Either[Set[UserId], SimpleUser]): IO[Checked[S]] =
    cell
      .flatMap(_.get).map: state =>
        (state.tokenToSession.get(token), idsOrUser) match
          case (None, _) =>
            val users = idsOrUser.fold(_.mkString("|"), _.id)
            logger.debug(s"🔒 InvalidSessionToken: Rejecting unknown $token of $users")
            Left(InvalidSessionTokenProblem)

          case (Some(session), Right(user))
            if !user.id.isAnonymous && user.id != session.currentUser.id =>
            tryUpdateLatelyAuthenticatedUser(user, session)

          case (Some(session), Left(userIds)) if !userIds.contains(session.currentUser.id) =>
            logger.debug("🔒 InvalidSessionToken: HTTPS distinguished name UserIds " +
              s"'${userIds.mkString(", ")}' do not include Sessions's ${session.currentUser.id}")
            Left(InvalidSessionTokenProblem)

          case (Some(session), _) =>
            Right(session)
      .flatTapT: session =>
        handleTimeout(session)
          .map: timedOutAndDeleted =>
            if timedOutAndDeleted then
              Left(InvalidSessionTokenProblem)
            else
              Right(())
      .flatTapT: session =>
        IO
          .unlessA(session.isEternal):
            session.touch(sessionTimeout)
          .as(Right(()))

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

  private object cleanUp:
    private val fiber = Atomic(none[FiberIO[Unit]])

    def scheduleNext: IO[Unit] =
      IO.whenA(fiber.get.isEmpty):
        cell.flatMap(_.get)
          .map(_.tokenToSession.values.exists(o => !o.isEternal))
          .map: nonEternalExists =>
            IO.whenA(nonEternalExists):
              cleanUp.delayBy(cleanupInterval)
                .startAndLogError
                .flatMap: fiber =>
                  setFiber(Some(fiber))

    private def cleanUp: IO[Unit] =
      cell.flatMap(_.get).flatMap: state =>
        logger.trace("cleanUp")
        state.tokenToSession.values.toVector.traverse(handleTimeout)
        setFiber(None) *> scheduleNext

    private def setFiber(value: Option[FiberIO[Unit]]): IO[Unit] =
      fiber.getAndSet(value)
        .fold(IO.unit)(_.cancel /*in case of race condition*/)

  /** true iff timed-out, then Session is deleted. */
  private def handleTimeout(session: S): IO[Boolean] =
    for
      isAlive <- session.isAlive
      r <-
        if isAlive then
          IO.False
        else
          for
            elapsed <- session.touchedBefore
            _ <- cell
              .flatMap(_.update(_.
                delete(session.sessionToken, s" timed out (last used ${elapsed.pretty} ago)")))
          yield true
    yield r

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


  private final case class State(tokenToSession: Map[SessionToken, S]):
    def delete(token: SessionToken, reason: String): State =
      tokenToSession.get(token) match
        case None => this
        case Some(session) =>
          logger.info(s"$token for ${session.currentUser.id}$reason")
          copy(tokenToSession = tokenToSession.removed(token))

    def logOpenSessions(): Unit =
      tokenToSession.values
        .groupMap(_.currentUser.id)(_.sessionToken)
        .view.mapValues(_.toVector.sortBy(_.number))
        .toVector.sortBy(_._1)
        .foreach: (userId, sessionTokens) =>
          logger.debug(sessionTokens.size.toString + " open sessions for " + userId +
            sessionTokens.view.mkString(" (", " ", ")"))


object SessionRegister:

  private val logger = Logger[this.type]

  def resource[S <: Session: Tag](newSession: SessionInit => S, config: Config)
  : ResourceIO[SessionRegister[S]] =
    Service.resource:
      new SessionRegister[S](newSession, config)

  // Does not stop the Actor !!!
  @TestOnly
  def forTest[S <: Session: Tag](newSession: SessionInit => S, config: Config)
  : SessionRegister[S] =
    new SessionRegister[S](newSession, config)

  val TestConfig: Config = config"""
    js7.component.name = "JS7 TEST"
    js7.auth.session.timeout = 1 minute
    """
