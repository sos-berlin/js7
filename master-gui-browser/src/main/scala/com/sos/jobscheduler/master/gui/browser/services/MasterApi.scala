package com.sos.jobscheduler.master.gui.browser.services

import com.sos.jobscheduler.base.auth.SessionToken
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.http.{HttpClientException, JsHttpClient}
import com.sos.jobscheduler.master.client.HttpMasterApi
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.gui.browser.common.Utils.stringToHtml
import com.sos.jobscheduler.master.gui.browser.services.JsBridge.guiConfig.buildId
import com.sos.jobscheduler.master.gui.browser.services.JsBridge.toastr
import japgolly.scalajs.react.Callback
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js
import scala.util.{Failure, Success, Try}

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

  def executeCommandCallback[C <: MasterCommand](
    command: C,
    onComplete: PartialFunction[Try[C#MyResponse], Unit] = PartialFunction.empty)
  : Callback =
    Callback.future(
      MasterApi.executeCommand(command)
        .materialize.map { tried ⇒
          def toast(level: String, msg: String) =
            toastr(level)(stringToHtml(command.toString) + "<br>" + stringToHtml(msg))
          val default: PartialFunction[Try[command.MyResponse], Unit] = {
            case Success(response)  ⇒ toast("success", response.toString)
            case Failure(throwable) ⇒ toast("error", throwable.toSimplifiedString)
          }
          onComplete.applyOrElse(tried, default)
          Callback.empty
        }
      .runAsync)
}
