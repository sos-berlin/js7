package js7.base.session

import cats.effect.Resource
import js7.base.auth.UserAndPassword
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.session.SessionApi.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.HttpClient
import js7.base.web.HttpClient.HttpException
import monix.eval.Task
import scala.concurrent.duration.*

// Test in SessionRouteTest
/**
  * @author Joacim Zschimmer
  */
trait SessionApi
{
  private val tryLogoutLock = AsyncLock("SessionApi.tryLogout")

  def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean = false)
  : Task[Completed]

  def logout(): Task[Completed]

  def clearSession(): Unit

  // overrideable
  def retryIfSessionLost[A](onError: Throwable => Task[Unit] = onErrorDefault)(body: Task[A])
  : Task[A] =
    body

  // overrideable
  def retryUntilReachable[A](onError: Throwable => Task[Boolean] = t => onErrorDefault(t).as(true))
    (body: => Task[A])
  : Task[A] =
    body

  final def tryLogout: Task[Completed] =
    Task.defer {
      logger.trace(s"↘ $toString: tryLogout")
      tryLogoutLock.lock(
        logout()
          .onErrorRecover { case t =>
            logger.debug(s"$toString: logout failed: ${t.toStringWithCauses}")
            clearSession()
            Completed
          }
          .guaranteeCase(exitCase => Task {
            logger.trace(s"↙ $toString: tryLogout => $exitCase")
          }))
    }

  protected[SessionApi] final def onErrorDefault(throwable: Throwable): Task[Unit] =
    Task {
      logger.debug(toString + ": " + throwable.toStringWithCauses)
    }

  def logError(throwable: Throwable): Task[Boolean] =
    SessionApi.logError(throwable, toString)
}

object SessionApi
{
  private val logger = scribe.Logger[this.type]

  /** Logs out when the resource is being released. */
  def resource[A <: SessionApi](api: Task[A]): Resource[Task, A] =
    Resource.make(
      acquire = api)(
      release = api =>
        api.logout()
          .void
          .onErrorHandle { t =>
            logger.debug(s"logout() => ${t.toStringWithCauses}")
            ()
          })

  def logError(throwable: Throwable, myToString: String): Task[Boolean] =
    Task {
      logger.warn(s"$myToString: ${throwable.toStringWithCauses}")
      throwable match {
        case _: javax.net.ssl.SSLException =>
        case _ =>
          if (throwable.getStackTrace.nonEmpty
            && throwable.getClass.scalaName != "org.apache.pekko.stream.StreamTcpException"
            && Option(throwable.getCause).forall(_.getClass.scalaName != "org.apache.pekko.stream.StreamTcpException")) {
            logger.debug(s"$myToString: ${throwable.toString}", throwable)
          }
      }
      true
    }

  trait NoSession extends SessionApi
  {
    final def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean = false) =
      Task.completed

    final def logout() =
      Task.completed

    final def clearSession() = {}
  }

  trait LoginUntilReachable extends SessionApi
  {
    protected def isTemporaryUnreachable(throwable: Throwable): Boolean =
      HttpClient.isTemporaryUnreachable(throwable)

    def hasSession: Boolean

    final def loginUntilReachable_(
      userAndPassword: Option[UserAndPassword],
      delays: Iterator[FiniteDuration] = defaultLoginDelays(),
      onError: Throwable => Task[Boolean] = logError,
      onlyIfNotLoggedIn: Boolean = false)
    : Task[Completed] =
      Task.defer {
        if (onlyIfNotLoggedIn && hasSession)
          Task.completed
        else
          login_(userAndPassword)
            .onErrorRestartLoop(()) { (throwable, _, retry) =>
              onError(throwable).flatMap(continue =>
                if (continue && delays.hasNext && isTemporaryUnreachable(throwable))
                  retry(()) delayExecution delays.next()
                else
                  Task.raiseError(throwable))
            }
      }

    def logErrorAndTerminate(throwable: Throwable): Task[Boolean] =
      logError(throwable).map(_ => false)
  }

  trait HasUserAndPassword extends LoginUntilReachable
  {
    protected def userAndPassword: Option[UserAndPassword]

    protected val loginDelays: () => Iterator[FiniteDuration] =
      () => defaultLoginDelays()

    override def retryIfSessionLost[A](onError: Throwable => Task[Unit] = onErrorDefault)
      (body: Task[A])
    : Task[A] =
      Task.defer {
        val delays = loginDelays()
        login(onlyIfNotLoggedIn = true) *>
          body
            .onErrorRestartLoop(()) {
              case (HttpException.HasProblem(problem), _, retry)
                if problem.is(InvalidSessionTokenProblem) && delays.hasNext =>
                clearSession()
                // Race condition with a parallel operation,
                // which after the same error has already logged-in again successfully.
                // Should be okay if login is delayed like here
                logger.debug(s"$toString: Login again due to: $problem")
                Task.sleep(delays.next()) *>
                  login() *>
                  retry(())

              case (throwable, _, _) =>
                Task.raiseError(throwable)
            }
      }

    override final def retryUntilReachable[A](onError: Throwable => Task[Boolean] = logError)
      (body: => Task[A])
    : Task[A] =
      Task.defer {
        val delays = loginDelays()
        loginUntilReachable(delays, onError = onError, onlyIfNotLoggedIn = true)
          .flatMap((_: Completed) =>
            body.onErrorRestartLoop(()) { (throwable, _, retry) =>
              (throwable match {
                case HttpException.HasProblem(problem)
                  if problem.is(InvalidSessionTokenProblem) && delays.hasNext =>
                  // Do not call onError on this minor problem
                  logger.debug(s"$toString: $problem")
                  loginUntilReachable(delays, onError = onError)

                case e: HttpException if isTemporaryUnreachable(e) && delays.hasNext =>
                  onError(e).flatMap(continue =>
                    if (continue)
                      loginUntilReachable(delays, onError = onError, onlyIfNotLoggedIn = true)
                    else
                      Task.raiseError(e))

                case _ =>
                  Task.raiseError(throwable)
              }) *>
                retry(()).delayExecution(delays.next())
            })
      }

    final def login(onlyIfNotLoggedIn: Boolean = false): Task[Completed] =
      login_(userAndPassword, onlyIfNotLoggedIn = onlyIfNotLoggedIn)

    final def loginUntilReachable(
      delays: Iterator[FiniteDuration] = defaultLoginDelays(),
      onError: Throwable => Task[Boolean] = logError,
      onlyIfNotLoggedIn: Boolean = false)
    : Task[Completed] =
      loginUntilReachable_(userAndPassword, delays, onError, onlyIfNotLoggedIn = onlyIfNotLoggedIn)
  }

  private val initialLoginDelays = {
    val seq = Seq(
      100.ms, 900.ms, 1.s, 1.s, 1.s, 2.s, 2.s, 2.s, // 10s
      5.s, 5.s, // 20s
      5.s, 5.s, // 30s
      5.s, 5.s, // 40s
      5.s, 5.s, // 50s
      5.s, 5.s) // 60s
    assert(seq.reduce(_ + _) == 1.minute)
    seq
  }

  def defaultLoginDelays(): Iterator[FiniteDuration] =
    initialLoginDelays.iterator ++ Iterator.continually(10.s)
}
