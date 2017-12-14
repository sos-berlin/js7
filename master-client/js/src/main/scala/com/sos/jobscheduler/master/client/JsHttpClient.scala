package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.master.client.HttpClientException.{HostUnreachable, HttpFailure, OtherFailure}
import io.circe
import io.circe.{Decoder, Encoder}
import org.scalajs.dom
import org.scalajs.dom.ext.{Ajax, AjaxException}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.math.min
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object JsHttpClient extends HttpClient {

  private val ReloadDelay = 1.second

  @js.native @JSGlobalScope
  object JavascriptGlobal extends js.Object {
    var indexHtmlJobschedulerVersionUuid: String = js.native
  }

  def get[A: Decoder](uri: String, timeout: Duration = Duration.Inf): Future[A] =
    decodeResponse(
      Ajax.get(
        uri,
        headers = Map("Accept" → "application/json"),
        timeout = if (timeout == Duration.Inf) 0 else min(timeout.toMillis, Int.MaxValue).toInt))

  def post[A: Encoder, B: Decoder](uri: String, data: A): Future[B] =
    decodeResponse(
      Ajax.post(uri, implicitly[Encoder[A]].apply(data).compactPrint, headers = Map("Content-Type" → "application/json")))

  private def decodeResponse[A: Decoder](body: ⇒ Future[dom.XMLHttpRequest]): Future[A] =
    for (xhr ← checkResponse(body)) yield circe.parser.decode[A](xhr.responseText) match {
      case Right(o) ⇒ o
      case Left(t) ⇒ logAndThrow(OtherFailure(s"Error in JSON decoding: ${t.toStringWithCauses}", Some(t)))
    }

  private def checkResponse[A](body: ⇒ Future[dom.XMLHttpRequest]): Future[dom.XMLHttpRequest] =
    body transform { tried ⇒ Success(checkResponse(tried)) }

  private def checkResponse(tried: Try[dom.XMLHttpRequest]): dom.XMLHttpRequest =
    tried match {
      case Failure(t: AjaxException) if t.xhr.statusText.nonEmpty ⇒
        logAndThrow(OtherFailure(s"Problem while accessing JobScheduler Master: $t ${t.xhr.statusText}\n${t.xhr.responseText}", Some(t)))

      case Failure(t: AjaxException) if t.getMessage == null || t.getMessage.isEmpty ⇒
        throw new HttpClientException(HostUnreachable())

      case Failure(t: AjaxException) ⇒
        throw new HttpClientException(HostUnreachable(t.toString))

      case Failure(t) ⇒
        logAndThrow(OtherFailure(s"Problem while accessing JobScheduler Master: ${t.toStringWithCauses}", Some(t)))

      case Success(xhr) ⇒
        if (Option(xhr.getResponseHeader("X-JobScheduler-Build-ID")).exists(_ != JavascriptGlobal.indexHtmlJobschedulerVersionUuid)) {
          reloadPage()  // Server version has changed. We cannot continue.
          logAndThrow(OtherFailure(s"JobScheduler Master version changed — Reloading page...", None))
        } else
          xhr
    }

  private def logAndThrow[A](error: HttpFailure): Nothing = {
    dom.console.warn(error.toString)
    throw new HttpClientException(error)
  }

  private def reloadPage(): Unit =
    dom.window.setTimeout(
      () ⇒ dom.window.location.reload(),
      ReloadDelay.toMillis)  // Delay in case of reload-loop
}
