package com.sos.jobscheduler.base.session

import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Problems.InvalidSessionTokenProblem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.web.HttpClient.HttpException
import io.circe.Decoder
import monix.eval.Task

trait HttpAutoRelogin
{
  this: SessionApi.LoginUntilReachable =>

  protected def userAndPassword: Option[UserAndPassword]

  protected def loginDelays() = Iterator(1.s, 1.s, 1.s, 2.s, 3.s, 5.s) ++ Iterator.continually(10.s)/*TODO*/

  final def retryUntilReachable[A: Decoder](body: => Task[A]): Task[A] =
    Task.defer {
      val delays = loginDelays()
      loginUntilReachable(userAndPassword, delays, onlyIfNotLoggedIn = true)
        .flatMap((_: Completed) =>
          body.onErrorRestartLoop(()) { (throwable, _, retry) =>
            (throwable match {
              case e: HttpException if delays.hasNext && e.problem.exists(_.codeOption contains InvalidSessionTokenProblem.code) =>
                scribe.warn(e.toStringWithCauses)
                loginUntilReachable(userAndPassword, delays)

              case e: HttpException if delays.hasNext && isUnreachable(e) =>
                scribe.warn(e.toStringWithCauses)
                loginUntilReachable(userAndPassword, delays, onlyIfNotLoggedIn = true)

              case _ =>
                Task.raiseError(throwable)
            }) >>
              retry(()).delayExecution(delays.next())
          })
    }

  final def relogin: Task[Completed] =
    loginUntilReachable(userAndPassword, loginDelays())
}
