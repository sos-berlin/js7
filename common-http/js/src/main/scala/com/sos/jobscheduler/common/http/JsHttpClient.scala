package com.sos.jobscheduler.common.http

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.http.HttpClientException.{HostUnreachable, HttpFailure, OtherFailure, Reason}
import com.sos.jobscheduler.common.http.JsHttpClient._
import io.circe
import io.circe.{Decoder, Encoder}
import monix.eval.Task
import org.scalajs.dom.ext.{Ajax, AjaxException}
import org.scalajs.dom.{XMLHttpRequest, window}
import scala.concurrent.duration._
import scala.math.min
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class JsHttpClient(requiredBuildId: String) extends HttpClient {

  def get[A: Decoder](uri: String, timeout: Duration = Duration.Inf): Task[A] =
    decodeResponse(
      Task.deferFuture(
        Ajax.get(
          uri,
          headers = Map("Accept" → "application/json"),
          timeout = if (timeout == Duration.Inf) 0 else min(timeout.toMillis, Int.MaxValue).toInt)))

  def post[A: Encoder, B: Decoder](uri: String, data: A): Task[B] =
    decodeResponse(post_(uri, data))

  def postDiscardResponse[A: Encoder](uri: String, data: A) =
    post_(uri, data).map(_.status)

  private def post_[A: Encoder](uri: String, data: A): Task[XMLHttpRequest] =
    Task.deferFuture(
      Ajax.post(
        uri,
        implicitly[Encoder[A]].apply(data).compactPrint,
        headers = Map("Content-Type" → "application/json")))

  private def decodeResponse[A: Decoder](body: ⇒ Task[XMLHttpRequest]): Task[A] =
    for (xhr ← checkResponse(body)) yield circe.parser.decode[A](xhr.responseText) match {
      case Right(o) ⇒ o
      case Left(t) ⇒ logAndThrow(OtherFailure(s"Error in JSON decoding: ${t.toStringWithCauses}", Some(t)))
    }

  private def checkResponse[A](body: ⇒ Task[XMLHttpRequest]): Task[XMLHttpRequest] =
    body.materialize.map(tried ⇒ Success(checkResponse(tried))).dematerialize

  private def checkResponse(tried: Try[XMLHttpRequest]): XMLHttpRequest =
    tried match {
      case Failure(t: AjaxException) if t.xhr.statusText.nonEmpty ⇒
        logAndThrow(OtherFailure(s"Problem while accessing JobScheduler: $t ${t.xhr.statusText}\n${t.xhr.responseText}", Some(t)))

      case Failure(t: AjaxException) if t.getMessage == null || t.getMessage.isEmpty ⇒
        throw new HttpClientException(HostUnreachable())

      case Failure(t: AjaxException) if t.xhr.status != 0 ⇒
        throw new HttpClientException(HttpFailure(t.xhr.status, t.xhr.statusText))

      case Failure(t: AjaxException) ⇒
        throw new HttpClientException(HostUnreachable(t.toString))

      case Failure(t) ⇒
        logAndThrow(OtherFailure(s"Problem while accessing JobScheduler: ${t.toStringWithCauses}", Some(t)))

      case Success(xhr) ⇒
        requireMatchingBuildId(xhr)
        xhr
    }

  private def requireMatchingBuildId(xhr: XMLHttpRequest): Unit =
    xhr.getResponseHeader("X-JobScheduler-Build-ID") match {
      case null ⇒
        logAndThrow(OtherFailure("JobScheduler does not respond as expected (missing buildId)", None))

      case fromServer if fromServer != requiredBuildId ⇒
        window.console.warn(s"JobScheduler build has changed from '$requiredBuildId' to '$fromServer' — Reloading page...")
        reloadPage()
        throw new HttpClientException(OtherFailure(s"JobScheduler build has changed — Reloading page...", None))

      case _ ⇒
    }

  private def logAndThrow[A](error: Reason): Nothing = {
    window.console.warn(error.toString)
    throw new HttpClientException(error)
  }

  private def reloadPage(): Unit =
    window.setTimeout(
      () ⇒ window.location.reload(),
      ReloadDelay.toMillis)  // Delay in case of reload-loop
}

object JsHttpClient {
  private val ReloadDelay = 2.second  // Just in case of a loop
}
