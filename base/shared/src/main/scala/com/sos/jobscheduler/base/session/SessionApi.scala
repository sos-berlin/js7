package com.sos.jobscheduler.base.session

import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Problems.InvalidSessionTokenProblem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, RichThrowable}
import com.sos.jobscheduler.base.web.HttpClient.HttpException
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration
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
  def retryable[A](body: => Task[A]): Task[A] =
    body

  final def logoutOrTimeout(timeout: FiniteDuration): Task[Completed] =
    logout()
      .timeout(timeout)
      .onErrorRecover { case t =>
        scribe.debug(s"$toString: logout failed: ${t.toStringWithCauses}")
        clearSession()
        Completed
      }
}

object SessionApi
{
  trait NoSession extends SessionApi
  {
    final def login_(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean = false) =
      Task.pure(Completed)

    final def logout() =
      Task.pure(Completed)

    final def clearSession() = {}
  }

  trait LoginUntilReachable extends SessionApi
  {
    protected def isTemporaryUnreachable(throwable: Throwable): Boolean

    def hasSession: Boolean

    final def loginUntilReachable_(
      userAndPassword: Option[UserAndPassword],
      delays: Iterator[FiniteDuration] = defaultLoginDelays(),
      onError: Throwable => Task[Boolean] = logThrowable,
      onlyIfNotLoggedIn: Boolean = false)
    : Task[Completed] =
      Task.defer {
        if (onlyIfNotLoggedIn && hasSession)
          Task.pure(Completed)
        else
          login_(userAndPassword)
            .onErrorRestartLoop(()) { (throwable, _, retry) =>
              onError(throwable).flatMap {
                case false => Task.raiseError(throwable)
                case true =>
                  (if (isTemporaryUnreachable(throwable) && delays.hasNext)
                    retry(()) delayExecution delays.next()
                  else
                    Task.raiseError(throwable))
              }
            }
      }

    protected def logThrowable(throwable: Throwable): Task[Boolean] =
      Task {
        scribe.warn(s"$toString: ${throwable.toStringWithCauses}")
        if (throwable.getStackTrace.nonEmpty && throwable.getClass.scalaName != "akka.stream.StreamTcpException") {
          scribe.debug(s"$toString: ${throwable.toString}", throwable)
        }
        true
      }
  }

  trait HasUserAndPassword extends LoginUntilReachable
  {
    protected def userAndPassword: Option[UserAndPassword]

    protected def loginDelays(): Iterator[FiniteDuration] =
      defaultLoginDelays()

    final def retryUntilReachable[A](body: => Task[A]): Task[A] =
      Task.defer {
        val delays = loginDelays()
        loginUntilReachable(delays, onlyIfNotLoggedIn = true)
          .flatMap((_: Completed) =>
            body.onErrorRestartLoop(()) { (throwable, _, retry) =>
              (throwable match {
                case e: HttpException if delays.hasNext && e.problem.exists(_.codeOption contains InvalidSessionTokenProblem.code) =>
                  scribe.debug(e.toStringWithCauses)
                  loginUntilReachable(delays)

                case e: HttpException if delays.hasNext && isTemporaryUnreachable(e) =>
                  scribe.warn(e.toStringWithCauses)
                  loginUntilReachable(delays, onlyIfNotLoggedIn = true)

                case _ =>
                  Task.raiseError(throwable)
              }) >>
                retry(()).delayExecution(delays.next())
            })
      }

    override def retryable[A](body: => Task[A]) =
      retryUntilReachable(body)

    final def login(onlyIfNotLoggedIn: Boolean = false)
    : Task[Completed] =
      login_(userAndPassword, onlyIfNotLoggedIn = onlyIfNotLoggedIn)

    final def loginUntilReachable(
      delays: Iterator[FiniteDuration] = defaultLoginDelays(),
      onError: Throwable => Task[Boolean] = logThrowable,
      onlyIfNotLoggedIn: Boolean = false)
    : Task[Completed] =
      loginUntilReachable_(userAndPassword, delays, onError, onlyIfNotLoggedIn = onlyIfNotLoggedIn)
  }

  def defaultLoginDelays(): Iterator[FiniteDuration] =
    Iterator(1.s, 1.s, 1.s, 2.s, 3.s, 5.s) ++ Iterator.continually(10.s)/*TODO*/
}
