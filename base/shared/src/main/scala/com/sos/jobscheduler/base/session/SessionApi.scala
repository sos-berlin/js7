package com.sos.jobscheduler.base.session

import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, RichThrowable}
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration
// Test in SessionRouteTest

/**
  * @author Joacim Zschimmer
  */
trait SessionApi
{
  def login(userAndPassword: Option[UserAndPassword], onlyIfNotLoggedIn: Boolean = false): Task[Completed]

  def logout(): Task[Completed]

  def clearSession(): Unit

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
  trait LoginUntilReachable extends SessionApi
  {
    protected def isUnreachable(throwable: Throwable): Boolean

    def hasSession: Boolean

    final def loginUntilReachable(
      userAndPassword: Option[UserAndPassword],
      delays: Iterator[FiniteDuration],
      onError: Throwable => Task[Unit] = logThrowable,
      onlyIfNotLoggedIn: Boolean = false)
    : Task[Completed] =
      Task.defer {
        if (onlyIfNotLoggedIn && hasSession)
          Task.pure(Completed)
        else
          login(userAndPassword)
            .onErrorRestartLoop(()) { (throwable, _, retry) =>
              onError(throwable) >>
                (if (isUnreachable(throwable) && delays.hasNext)
                  retry(()) delayExecution delays.next()
                else
                  Task.raiseError(throwable))
            }
      }

    protected def logThrowable(throwable: Throwable): Task[Unit] =
      Task {
        scribe.warn(s"$toString: ${throwable.toStringWithCauses}")
        if (throwable.getStackTrace.nonEmpty && throwable.getClass.scalaName != "akka.stream.StreamTcpException") {
          scribe.debug(s"$toString: ${throwable.toString}", throwable)
        }
      }
  }
}
