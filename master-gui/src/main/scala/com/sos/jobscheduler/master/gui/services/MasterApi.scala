package com.sos.jobscheduler.master.gui.services

import com.sos.jobscheduler.data.event.{EventId, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{Order, OrderEvent}
import com.sos.jobscheduler.master.data.MasterOverview
import io.circe
import io.circe.Decoder
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax.InputData
import org.scalajs.dom.ext.{Ajax, AjaxException}
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object MasterApi {
  type Response[A] = Either[Error, A]
  private val ReloadDelay = 1.second

  @js.native @JSGlobalScope
  object JavascriptGlobal extends js.Object {
    var indexHtmlJobschedulerVersionUuid: String = js.native
  }

  def executeCommand(command: String): Future[Response[Unit]] =
    post("api", command)

  def overview: Future[Response[MasterOverview]] =
    get[MasterOverview]("api")

  def orders: Future[Response[Stamped[Seq[Order[Order.State]]]]] =
    get[Stamped[Seq[Order[Order.State]]]]("api/order/?return=Order")

  def events(after: EventId, timeout: Duration): Future[Response[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]]] =
    get[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]](
      s"api/order/?return=OrderEvent&timeout=${timeout.toMillis}ms&after=$after")

  private def post[A](uri: String, data: InputData): Future[Response[Unit]] =
    checkResponse(
      Ajax.post(uri, data, headers = Map("Content-Type" → "application/json"))) map (_ ⇒ Right(()))

  private def get[A: Decoder](uri: String): Future[Response[A]] =
    decodeResponse(
      Ajax.get(uri, headers = Map("Accept" → "application/json")),
      xhr ⇒ circe.parser.decode[A](xhr.responseText) match {
        case Right(o) ⇒ Right(o)
        case Left(t) ⇒ logLeft(OtherError(s"Error in JSON decoding: $t"))
      })

  private def decodeResponse[A: Decoder](body: ⇒ Future[dom.XMLHttpRequest], decode: dom.XMLHttpRequest ⇒ Response[A]): Future[Response[A]] =
    for (response ← checkResponse(body)) yield
      for {
        xhr ← response
        a ← decode(xhr)
      } yield a

  private def checkResponse[A](body: ⇒ Future[dom.XMLHttpRequest]): Future[Response[dom.XMLHttpRequest]] =
    body transform { tried ⇒ Success(checkResponse(tried)) }

  private def checkResponse(tried: Try[dom.XMLHttpRequest]): Response[dom.XMLHttpRequest] =
    tried match {
      case Failure(t: AjaxException) if t.xhr.statusText.nonEmpty ⇒
        logLeft(OtherError(s"Problem while accessing JobScheduler Master: $t ${t.xhr.statusText}\n${t.xhr.responseText}"))

      case Failure(t: AjaxException) if t.getMessage == null || t.getMessage.isEmpty ⇒
        Left(HostUnreachable(""))

      case Failure(t: AjaxException) ⇒
        Left(HostUnreachable(t.toString))

      case Failure(t) ⇒
        logLeft(OtherError(s"Problem while accessing JobScheduler Master: $t"))

      case Success(xhr) ⇒
        if (Option(xhr.getResponseHeader("X-JobScheduler-Build-ID")).exists(_ != JavascriptGlobal.indexHtmlJobschedulerVersionUuid)) {
          dom.window.setTimeout(() ⇒ dom.window.location.reload(), ReloadDelay.toMillis)  // Delay in case of reload-loop
          logLeft(OtherError(s"JobScheduler Master version changed — Reloading page..."))
        } else
          Right(xhr)
    }

  private def logLeft[A](error: Error): Left[Error, A] = {
    dom.console.warn(error.toString)
    Left(error)
  }

  sealed trait Error

  final case class HostUnreachable(reason: String) extends Error {
    override def toString = s"⚠️ JobScheduler Master is not reachable — $reason".trim stripSuffix " —"
  }

  final case class OtherError(reason: String) extends Error {
    override def toString = reason
  }
}
