package com.sos.jobscheduler.master.gui.browser.services

import com.sos.jobscheduler.base.auth.SessionToken
import com.sos.jobscheduler.common.http.{HttpClientException, JsHttpClient}
import com.sos.jobscheduler.master.client.HttpMasterApi
import com.sos.jobscheduler.master.gui.browser.services.JsBridge.guiConfig.buildId
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
object MasterApi extends HttpMasterApi with JsHttpClient
{
  protected def baseUriString = "/"

  protected val requiredBuildId = buildId.ensuring(_.nonEmpty, "GUI error: Variable jobschedulerBuildId is empty")

  protected def httpClient = this

  protected def credentialsHeaders = Nil  // Browser handles authentication with a user dialog

  def loginUntilReachable(afterErrorDelay: Iterator[FiniteDuration]): Task[SessionToken] =
    loginUntilReachable(afterErrorDelay, afterUnauthorized = false)

  private def loginUntilReachable(afterErrorDelay: Iterator[FiniteDuration], afterUnauthorized: Boolean): Task[SessionToken] =
    logout().flatMap(_ ⇒ login(None)).attempt flatMap {
      case Left(e: HttpClientException) ⇒
        if (e.reason.isUnreachable)
          loginUntilReachable(afterErrorDelay, afterUnauthorized = false).delayExecution(afterErrorDelay.next())
        else
          e.reason match {
            case reason: HttpClientException.HttpFailure if reason.status == 401/*Unauthorized*/ && !afterUnauthorized ⇒
              clearSession()
              loginUntilReachable(afterErrorDelay, afterUnauthorized = true).delayExecution(afterErrorDelay.next())

            case _ ⇒ throw e
          }

      case Left(t) ⇒ throw t
      case Right(o) ⇒ Task.pure(o)  // Success
    }
}
