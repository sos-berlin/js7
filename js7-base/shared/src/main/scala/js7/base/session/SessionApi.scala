package js7.base.session

import js7.base.auth.UserAndPassword
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.session.SessionApi._
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.HttpClient
import js7.base.web.HttpClient.HttpException
import monix.eval.Task
import scala.concurrent.duration._

// Test in SessionRouteTest
/**
  * @author Joacim Zschimmer
  */
trait SessionApi
{
  def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean = false): Task[Completed]

  def logout(): Task[Completed]

  def clearSession(): Unit

  // overrideable
  def retryUntilReachable[A](onError: Throwable => Task[Boolean] = onErrorDoNothing)(body: => Task[A]): Task[A] =
    body

  final def tryLogout: Task[Completed] =
    Task.defer {
      scribe.trace(s"$toString: tryLogout")
      logout()
        .onErrorRecover { case t =>
          scribe.debug(s"$toString: logout failed: ${t.toStringWithCauses}")
          clearSession()
          Completed
        }
        .guaranteeCase(exitCase => Task {
          scribe.trace(s"$toString: tryLogout => $exitCase")
        })
  }
}

object SessionApi
{
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

    def logError(throwable: Throwable): Task[Boolean] =
      Task {
        scribe.warn(s"$toString: ${throwable.toStringWithCauses}")
        throwable match {
          case _: javax.net.ssl.SSLException =>
          case _ =>
            if (throwable.getStackTrace.nonEmpty && throwable.getClass.scalaName != "akka.stream.StreamTcpException") {
              scribe.debug(s"$toString: ${throwable.toString}", throwable)
            }
        }
        true
      }
  }

  trait HasUserAndPassword extends LoginUntilReachable
  {
    protected def userAndPassword: Option[UserAndPassword]

    protected val loginDelays: () => Iterator[FiniteDuration] =
      () => defaultLoginDelays()

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
                  scribe.debug(problem.toString)
                  loginUntilReachable(delays, onError = onError)

                case e: HttpException if isTemporaryUnreachable(e) && delays.hasNext =>
                  onError(e).flatMap(continue =>
                    if (continue)
                      loginUntilReachable(delays, onError = onError, onlyIfNotLoggedIn = true)
                    else
                      Task.raiseError(e))

                case _ =>
                  Task.raiseError(throwable)
              }) >>
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
    val seq = Seq(1.s, 1.s, 1.s, 1.s, 1.s, 2.s, 3.s,  5.s, 5.s,  5.s, 5.s,  5.s, 5.s,  5.s, 5.s,  5.s, 5.s)
    assert(seq.reduce(_ + _) == 1.minute)
    seq
  }

  def defaultLoginDelays(): Iterator[FiniteDuration] =
    initialLoginDelays.iterator ++ Iterator.continually(10.s)

  private def onErrorDoNothing(throwable: Throwable) = Task.pure(false)
}
