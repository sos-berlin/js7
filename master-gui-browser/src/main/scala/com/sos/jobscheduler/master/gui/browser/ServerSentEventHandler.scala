package com.sos.jobscheduler.master.gui.browser

import cats.syntax.option._
import com.sos.jobscheduler.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.http.JsHttpClient
import com.sos.jobscheduler.data.event.{EventId, EventRequest, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.master.gui.browser.common.Utils.toUriQueryString
import com.sos.jobscheduler.master.gui.browser.components.state.{GuiState, OrdersState}
import com.sos.jobscheduler.master.gui.browser.services.JsBridge.guiConfig
import io.circe.Json
import japgolly.scalajs.react.{BackendScope, Callback, CallbackTo}
import org.scalajs.dom.raw.EventSource
import org.scalajs.dom.window
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.scalajs.js
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class ServerSentEventHandler(protected val scope: BackendScope[GuiComponent.Props, GuiState]) extends EventHandler
{
  private val buffer = new js.Array[Stamped[KeyedEvent[OrderEvent]]]()
  private var timer = none[Int]

  protected def fetchAndHandleEvents(
    after: EventId,
    forStep: Int,
    timeout: FiniteDuration,
    afterErrorDelay: Iterator[FiniteDuration]): Callback
  = Callback {
    var lastEventId = after
    _isRequestingEvents = true
    val eventSource = new EventSource(s"master/api/event?v=${guiConfig.buildId}&" +
      toUriQueryString(
        EventRequest.singleClass[OrderEvent](
          after = after,
          timeout = new FiniteDuration(guiConfig.sseServerTimeout, MILLISECONDS))
        .toQueryParameters))

    eventSource.onopen = { _ ⇒
      scope.modState(_.copy(isConnected = true)).runNow()
    }

    eventSource.onerror = { _ ⇒
      //window.console.info(s"EventSource.onerror ${js.typeOf(evt)}: ${js.JSON.stringify(evt)}")
      eventSource.close()
      _isRequestingEvents = false
      scope.state.flatMap(state ⇒
        scope.setState(state.copy(isConnected = false)) >>
          startRequestAndHandleEvents(after = lastEventId, forStep = forStep/*???*/, afterErrorDelay = afterErrorDelay)
            .delay(afterErrorDelay.next())
      ).runNow()
    }

    eventSource.onmessage = message ⇒ try {
      val json = message.data.asInstanceOf[String].parseJson
      if (json.jsonObjectOrThrow("TYPE") contains Json.fromString("Problem")) {
        eventSource.close()
        _isRequestingEvents = false
        handleProblem(json.as[Problem].orThrow)
      } else {
        val stampedEvent = json.as[Stamped[KeyedEvent[OrderEvent]]].orThrow
        scope.state
          .flatMap(state ⇒
            ifInactive(state) map (onInactive ⇒
              Callback {
                eventSource.close()
                _isRequestingEvents = false
              } >> onInactive
            ) getOrElse {
              buffer += stampedEvent
              if (buffer.length < guiConfig.sseBatchSize) {
                Callback {
                  timer foreach window.clearTimeout
                  timer = Some(
                    window.setTimeout(() ⇒ {
                      timer = None
                      handleEvents(buffer.toVector).runNow()
                      lastEventId = stampedEvent.eventId
                      buffer.clear()
                    },
                    timeout = guiConfig.sseBatchDelay))
                }
              } else {
                val stampedEvents = buffer.toVector
                buffer.clear()
                handleEvents(stampedEvents) >>
                  Callback { lastEventId = stampedEvent.eventId }
              }
            })
          .attemptTry.map {
            case Success(o) ⇒ o
            case Failure(t) ⇒
              eventSource.close()
              _isRequestingEvents = false
              onGuiFailed(t)
        }
        .runNow()
      }
    } catch {
      case NonFatal(t) ⇒
        onGuiFailed(t)
        throw t;
    }
  }

  private def handleEvents(stampeds: Seq[Stamped[KeyedEvent[OrderEvent]]]): CallbackTo[Unit] =
    scope.modState(state ⇒ state.copy(
      ordersState = state.ordersState.copy(
        content = state.ordersState.content match {
          case content: OrdersState.FetchedContent ⇒
            content.handleEvents(stampeds)
          case o ⇒ o  // Ignore the events
        }
      ))
    )

  private def handleProblem(problem: Problem): Unit =
    if (problem.toString == "BUILD-CHANGED"/*See EventRoute*/) {
      window.console.warn(s"JobScheduler build has changed — Reloading page...")
      JsHttpClient.reloadPage()
    } else
      throw problem.throwable
}
