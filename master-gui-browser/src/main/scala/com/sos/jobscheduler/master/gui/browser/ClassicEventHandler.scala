package com.sos.jobscheduler.master.gui.browser

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.master.gui.browser.EventHandler.{AfterTimeoutDelay, ContinueDelay, FirstEventTimeout, TornDelay}
import com.sos.jobscheduler.master.gui.browser.components.state.{AppState, GuiState, OrdersState}
import com.sos.jobscheduler.master.gui.browser.services.JsBridge.guiConfig
import com.sos.jobscheduler.master.gui.browser.services.MasterApi
import japgolly.scalajs.react.{BackendScope, Callback}
import java.util.concurrent.TimeUnit.SECONDS
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom.window
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class ClassicEventHandler(protected val scope: BackendScope[GuiComponent.Props, GuiState]) extends EventHandler
{
  private var tornDelay = Duration.Zero
  private var tornTimer: Option[Int] = None

  protected def fetchAndHandleEvents(
    after: EventId,
    forStep: Int,
    timeout: FiniteDuration,
    afterErrorDelay: Iterator[FiniteDuration])
  : Callback =
  {
    def handleResponse(response: Try[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]]): Callback =
      catching {
        withProperState(forStep) {
          case state if state.appState == AppState.Freezed ⇒
            Callback.empty  // Discard response

          case state ⇒
            val step = state.ordersState.step
            response match {
              case Failure(t) ⇒
                window.console.warn(t.toStringWithCauses)
                scope.setState(state.copy(
                  isConnected = false)
                ) >>
                  Callback.future[Unit] {
                    MasterApi.loginUntilReachable(afterErrorDelay).runAsync map (_ ⇒
                      startRequestAndHandleEvents(after = after, forStep = step, timeout = FirstEventTimeout, afterErrorDelay = afterErrorDelay))
                  }.delay(afterErrorDelay.next()).void

              case Success(EventSeq.Empty(lastEventId)) ⇒
                scope.setState(state.copy(
                  isConnected = true)
                ) >>
                  startRequestAndHandleEvents(after = lastEventId, forStep = step)
                    .delay(AfterTimeoutDelay).void

              case Success(EventSeq.NonEmpty(stampedEvents)) ⇒
                val nextStep = step + 1
                scope.setState(state.copy(
                  isConnected = true,
                  ordersState = state.ordersState.copy(
                    content = state.ordersState.content match {
                      case content: OrdersState.FetchedContent ⇒
                        content.handleEvents(stampedEvents)
                      case o ⇒ o  // Ignore the events
                    },
                    step = nextStep))
                ) >>
                  startRequestAndHandleEvents(after = stampedEvents.last.eventId, forStep = nextStep)
                    .delay(ContinueDelay).void

              case Success(_: TearableEventSeq.Torn) ⇒
                window.console.info("TearableEventSeq.Torn")
                requestStateAndEvents.delay(TornDelay).void  // Request all orders
            }
        }
      }

    Callback.future {
      _isRequestingEvents = true
      MasterApi.events(EventRequest.singleClass[OrderEvent](after = after, timeout = timeout, limit = 1000/*limit Master's memory usage*/,
          tornOlder = Duration(guiConfig.tornOlderSeconds, SECONDS)))
        .runAsync
        .andThen { case _ ⇒
          _isRequestingEvents = false  // TODO Falls requestStateAndEvents() aufgerufen wird, während Events geholt werden, wird _isRequestingEvents zu früh zurückgesetzt (wegen doppelter fetchAndHandleEvents)
        }
        .transform(o ⇒ Success(handleResponse(o)))
    }
  }

  private def withProperState(forStep: Int)(body: GuiState ⇒ Callback): Callback =
    for {
      state ← scope.state
      callback ←
        if (state.ordersState.step == forStep)
          catching {
            body(state)
          }
        else
          Callback.log(s"${state.appState} forStep=$forStep != state.version=${state.ordersState.step} - Response discarded")
    } yield callback

  private def catching(body: ⇒ Callback): Callback =
    try body
    catch {
      case t: Throwable ⇒ onGuiFailed(t)
    }
}
