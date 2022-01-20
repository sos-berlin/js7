package js7.data.session

import js7.base.BuildInfo
import js7.base.auth.{SessionToken, UserAndPassword}
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.session.SessionCommand.{Login, Logout}
import js7.base.session.{HasSessionToken, SessionApi, SessionCommand}
import js7.base.time.Stopwatch.{bytesPerSecondString, itemsPerSecondString}
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.version.Version
import js7.base.web.{HttpClient, Uri}
import js7.data.event.SnapshotableState
import js7.data.session.HttpSessionApi._
import monix.eval.Task
import monix.execution.atomic.AtomicAny
import scala.concurrent.duration.Deadline.now
// Test in SessionRouteTest

/**
  * @author Joacim Zschimmer
  */
trait HttpSessionApi extends SessionApi.HasUserAndPassword with HasSessionToken
{
  protected def httpClient: HttpClient
  protected def sessionUri: Uri

  private val lock = AsyncLock("HttpSessionApi")
  private val sessionTokenRef = AtomicAny[Option[SessionToken]](None)

  protected final def logOpenSession(): Unit =
    for (token <- sessionTokenRef.get()) {
      logger.debug(s"close(), but $token not logged-out: $toString")
    }

  final def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean = false)
  : Task[Completed] =
    lock.lock(
      Task.defer {
        if (onlyIfNotLoggedIn && hasSession)
          Task.completed
        else {
          val cmd = Login(userAndPassword, Some(BuildInfo.version))
          Task { logger.debug(s"$toString: $cmd") } >>
          executeSessionCommand(cmd)
            .map { response =>
              for (problem <- checkNonMatchingVersion(response.js7Version).left) {
                logger.error(problem.toString)
              }
              setSessionToken(response.sessionToken)
              for (version <- response.js7Version) if (version != BuildInfo.version) {
                logger.info(sessionUri.string.takeWhile(_ != '/') +
                  s" server version $version differs from own version ${BuildInfo.version}")
              }
              Completed
            }
        }
      })

  final def logout(): Task[Completed] =
    lock.lock(
      Task.defer {
        sessionTokenRef.get() match {
          case None => Task.completed
          case sometoken @ Some(sessionToken) =>
            Task.defer {
              val cmd = Logout(sessionToken)
              logger.debug(s"$toString: $cmd ${userAndPassword.fold("")(_.userId.string)}")
              executeSessionCommand(cmd, suppressSessionToken = true)
                .doOnFinish(_ => Task {
                  // Change nothing in case of a concurrent successful Logout or Login
                  sessionTokenRef.compareAndSet(sometoken, None)
                })
                .map((_: SessionCommand.Response.Accepted) => Completed)
                .logWhenItTakesLonger(s"logout $httpClient")
            }
        }
      })

  private def executeSessionCommand(command: SessionCommand, suppressSessionToken: Boolean = false)
  : Task[command.Response] = {
    implicit val implicitSessionToken =
      if (suppressSessionToken) Task.pure(None)
      else Task { sessionToken }
    httpClient.post[SessionCommand, SessionCommand.Response](sessionUri, command)
      .map(_.asInstanceOf[command.Response])
  }

  final def clearSession(): Unit =
    sessionTokenRef := None

  final def setSessionToken(sessionToken: SessionToken): Unit =
    sessionTokenRef := Some(sessionToken)

  final def sessionToken: Option[SessionToken] =
    sessionTokenRef.get()

  protected final def snapshotAs[S <: SnapshotableState[S]](uri: Uri)
    (implicit S: SnapshotableState.Companion[S])
  : Task[S] =
    Task.defer {
      val startedAt = now
      httpClient.getRawLinesObservable(uri)
        .logTiming(_.length, startedAt = startedAt, onComplete = (d, n, exitCase) =>
          logger.debug(s"$S snapshot receive $exitCase - ${bytesPerSecondString(d, n)}"))
        .map(_
          .mapParallelBatch()(_
            .parseJsonAs(S.snapshotObjectJsonCodec).orThrow))
        .logTiming(startedAt = startedAt, onComplete = (d, n, exitCase) =>
          logger.debug(s"$S snapshot receive $exitCase - ${itemsPerSecondString(d, n, "objects")}"))
        .flatMap(S.fromObservable)
    }
}

object HttpSessionApi {
  private val logger = scribe.Logger[this.type]

  private[session] def checkNonMatchingVersion(
    serverVersion: Option[String],
    ourVersion: String = BuildInfo.version)
  : Checked[Unit] =
    Checked.catchNonFatal {
      serverVersion match {
        case None => Left(Problem.pure("Server did not return its version"))
        case Some(serverVersion) =>
          for {
            client <- Version.checked(serverVersion)
            our <- Version.checked(ourVersion)
            _ <-
              (client.major == our.major && client.minor == our.minor) !!
                Problem(
                  s"Server version $serverVersion does not match this client version $our")
          } yield ()
      }
    }.flatten
}
