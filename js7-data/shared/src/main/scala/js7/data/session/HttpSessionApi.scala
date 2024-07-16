package js7.data.session

import cats.effect.{IO, Outcome}
import cats.syntax.option.*
import js7.base.Js7Version
import js7.base.auth.{SessionToken, UserAndPassword}
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.convert.As.StringAsBoolean
import js7.base.fs2utils.StreamExtensions.*
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.session.SessionCommand.{Login, Logout}
import js7.base.session.{HasSessionToken, SessionApi, SessionCommand}
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.{AsyncLock, Atomic}
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.RichAny
import js7.base.utils.Tests.isTest
import js7.base.version.Js7Versions.checkNonMatchingVersion
import js7.base.version.Version
import js7.base.web.HttpClient.HttpException
import js7.base.web.{HttpClient, Uri}
import js7.data.event.SnapshotableState
import js7.data.session.HttpSessionApi.*
import scala.concurrent.duration.Deadline.now

// Test in SessionRouteTest
/**
  * @author Joacim Zschimmer
  */
trait HttpSessionApi extends SessionApi, HasSessionToken:

  protected def httpClient: HttpClient
  protected def sessionUri: Uri

  private val lock = AsyncLock("HttpSessionApi")
  private val sessionTokenRef = Atomic(none[SessionToken])

  protected final def logOpenSession(): Unit =
    for token <- sessionTokenRef.get() do
      logger.debug(s"close(), but $token not logged-out: $toString")

  final def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean = false)
  : IO[Completed] =
    IO.defer(
      if onlyIfNotLoggedIn && hasSession then
        IO.completed // avoid lock logging
      else
        lock.lock(IO.defer(
          if onlyIfNotLoggedIn && hasSession then
            IO.completed
          else {
            val cmd = Login(userAndPassword, Some(Js7Version))
            logger.debug(s"$toString: $cmd")
            executeSessionCommand(cmd)
              .pipeIf(isPasswordLoggable)(_.guaranteeCase {
                case Outcome.Errored(t: HttpException) if t.statusInt == 401/*Unauthorized*/ =>
                  // Logs the password !!!
                  IO(logger.debug(
                    s"⛔️ Login ${userAndPassword.map(o => s"${o.userId} »${o.password.string}«")} => ${t.problem getOrElse t}"))
                case _ => IO.unit
              })
              .map(response => {
                logNonMatchingVersion(
                  otherVersion = response.js7Version,
                  otherName = sessionUri.stripPath.toString)
                setSessionToken(response.sessionToken)
                Completed
              })
          })))

  final def logout(): IO[Completed] =
    IO.defer:
      if sessionTokenRef.get().isEmpty then
        IO.completed // avoid lock logging
      else
        lock.lock(IO.defer:
          sessionTokenRef.get() match
            case None => IO.completed
            case sometoken @ Some(sessionToken) =>
              val cmd = Logout(sessionToken)
              logger.debug(s"$toString: $cmd")
              executeSessionCommand(cmd, suppressSessionToken = true)
                .guarantee(IO:
                  // Change nothing in case of a concurrent successful Logout or Login
                  sessionTokenRef.compareAndSet(sometoken, None))
                .map((_: SessionCommand.Response.Accepted) => Completed)
                .logWhenItTakesLonger(s"logout $httpClient"))

  private def executeSessionCommand(command: SessionCommand, suppressSessionToken: Boolean = false)
  : IO[command.Response] =
    given IO[Option[SessionToken]] =
      if suppressSessionToken then IO.pure(None)
      else IO { sessionToken }
    httpClient.post[SessionCommand, SessionCommand.Response](sessionUri, command)
      .map(_.asInstanceOf[command.Response])

  final def clearSession(): Unit =
    sessionTokenRef := None

  final def setSessionToken(sessionToken: SessionToken): Unit =
    sessionTokenRef := Some(sessionToken)

  final def sessionToken: Option[SessionToken] =
    sessionTokenRef.get()

  protected final def snapshotAs[S <: SnapshotableState[S]](uri: Uri)
    (implicit S: SnapshotableState.Companion[S])
  : IO[S] =
    IO.defer:
      val startedAt = now
      httpClient.getRawLinesStream(uri)
        .map(_
          .logTiming(_.length, startedAt = startedAt, onComplete = (d, n, exitCase) => IO:
            logger.debug(s"$S snapshot receive $exitCase - ${bytesPerSecondString(d, n)}"))
          .mapParallelBatch()(_
            .parseJsonAs(S.snapshotObjectJsonCodec).orThrow)
          .logTiming(startedAt = startedAt, onComplete = (d, n, exitCase) => IO:
            logger.debug:
              s"$S snapshot receive $exitCase - ${itemsPerSecondString(d, n, "objects")}"))
        .flatMap(S.fromStream)


object HttpSessionApi:
  private val logger = Logger[this.type]
  private val isPasswordLoggable = isTest &&
    sys.props.get("js7.test.log-password").fold(false)(StringAsBoolean.apply)

  private[session] def logNonMatchingVersion(
    otherVersion: Version,
    otherName: => String,
    ourVersion: Version = Js7Version): Unit =
    checkNonMatchingVersion(otherVersion, otherName = otherName, ourVersion = ourVersion) match
      case Left(problem) => logger.error(problem.toString)
      case Right(()) =>
        if otherVersion != ourVersion then
          logger.info(s"$otherName server version $otherVersion differs from own version $ourVersion")
