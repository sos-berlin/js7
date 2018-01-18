package com.sos.jobscheduler.master.gui

import com.sos.jobscheduler.base.utils.Collections.RichMap
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.data.event.{EventId, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{Order, OrderEvent}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.gui.GuiBackend._
import com.sos.jobscheduler.master.gui.GuiRenderer.Moon
import com.sos.jobscheduler.master.gui.ScreenBackground.setScreenClass
import com.sos.jobscheduler.master.gui.common.Utils.isMobile
import com.sos.jobscheduler.master.gui.components.state.{AppState, GuiState, OrdersState, PreparedWorkflow}
import com.sos.jobscheduler.master.gui.services.MasterApi
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, Callback}
import org.scalajs.dom.{raw, window}
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class GuiBackend(scope: BackendScope[GuiComponent.Props, GuiState]) {

  private val onDocumentVisibilityChanged = (_: raw.Event) ⇒ awake().runNow()
  private val onHashChanged = { _: raw.HashChangeEvent⇒
    //dom.window.screenX
    scope.modState(state ⇒ state.updateUriHash)
      //.memoizePositionForUri(event.oldURL))
    .runNow()
  }
  private var isRequestingEvents = false

  def componentDidMount() =
    Callback {
      window.document.addEventListener("visibilitychange", onDocumentVisibilityChanged)
      window.onhashchange = onHashChanged
    } >>
      scope.modState(o ⇒ o.copy(
        ordersState = o.ordersState.copy(
          content = OrdersState.FetchingContent))) >>
      requestWorkflows >>
      requestOrdersAndEvents

  def componentWillUnmount() = Callback {
    window.document.removeEventListener("visibilitychange", onDocumentVisibilityChanged, false)
  }

  def componentDidUpdate(): Callback =
    for (state ← scope.state) yield setScreenClass(state)

  private def requestWorkflows: Callback =
    Callback.future {
      MasterApi.workflowScripts transform {
        case Failure(err) ⇒ setError(err)
        case Success(stamped: Stamped[Seq[Workflow.Named]]) ⇒
          Try {
            scope.modState(_.copy(
              pathToWorkflow = stamped.value.map(o ⇒ o.path → PreparedWorkflow(o.path, o.workflow)).toMap
                .withNoSuchKey(k ⇒ throw new NoSuchElementException(s"Unknown $k"))))
          }
      }
    }

  def requestOrdersAndEvents: Callback =
    for {
      state ← scope.state
      callback ← requestOrdersAndEvents2.when(state.appState == AppState.RequestingEvents).void
    } yield callback

  private def requestOrdersAndEvents2: Callback =
    scope.modState(o ⇒ o.copy(
      ordersState = o.ordersState.copy(
        content = OrdersState.FetchingContent))
    ) >>
    Callback.future {
      MasterApi.orders transform {
        case Failure(err) ⇒ setError(err)
        case Success(stamped: Stamped[Seq[Order[Order.State]]]) ⇒
          Try {
            for {
              state ← scope.state
              _ ← scope.setState(state.copy(
                  isConnected = true,
                  ordersState = state.ordersState.updateOrders(stamped)))
              callback ← requestAndHandleEvents(after = stamped.eventId, forStep = state.ordersState.step + 1)
            } yield callback
          }
      }
    }

  private def setError(throwable: Throwable) =
    Try {
      scope.modState(state ⇒ state.copy(
        isConnected = false,
        ordersState = state.ordersState.copy(
          content = InitialFetchedContext,
          error = Some(throwable.toString),
          step = state.ordersState.step + 1)))
    }


  private def requestAndHandleEvents(
    after: EventId,
    forStep: Int,
    timeout: FiniteDuration = EventTimeout,
    afterErrorDelay: Iterator[FiniteDuration] = newAfterErrorDelayIterator)
  : Callback = {
      def fetchEvents() =
        Callback.future {
          isRequestingEvents = true
          MasterApi.orderEvents(after = after, timeout = timeout)
            .andThen { case _ ⇒
              isRequestingEvents = false  // TODO Falls requestOrdersAndEvents() aufgerufen wird, während Events geholt werden, wird isRequestingEvents zu früh zurückgesetzt (wegen doppelter fetchEvents)
            }
            .transform (o ⇒ Success(handleResponse(o)))
        }

      def handleResponse(response: Try[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]]): Callback =
        catching {
          withProperState(forStep) {
            case state if state.appState == AppState.Freezed ⇒
              Callback.empty  // Discard response

            case state ⇒
              val step = state.ordersState.step
              response match {
                case Failure(_) ⇒
                  scope.setState(state.copy(
                    isConnected = false)
                  ) >>
                    requestAndHandleEvents(after = after, forStep = step, timeout = FirstEventTimeout, afterErrorDelay = afterErrorDelay)
                      .delay(afterErrorDelay.next()).void

                case Success(EventSeq.Empty(lastEventId)) ⇒
                  scope.setState(state.copy(
                    isConnected = true)
                  ) >>
                    requestAndHandleEvents(after = lastEventId, forStep = step)
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
                    requestAndHandleEvents(after = stampedEvents.last.eventId, forStep = nextStep)
                      .delay(ContinueDelay).void

                case Success(EventSeq.Torn) ⇒
                  window.console.warn("EventSeq.Torn")
                  requestOrdersAndEvents.delay(TornDelay).void  // Request all orders
              }
          }
        }

      for {
        state ← scope.state
        callback ←
          if (state.appState != AppState.RequestingEvents)
            Callback.empty
          else if (window.document.hidden) {
            window.console.log(s"$Moon Standby...")
            scope.modState(_.copy(
              appState = AppState.Standby))
          } else
            fetchEvents()
      } yield callback
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
          Callback.log(s"${state.appState} forStep=$forStep!= state.version=${state.ordersState.step} - Response discarded")
    } yield callback

  private def catching(body: ⇒ Callback): Callback =
    try body
    catch {
      case t: Throwable ⇒
        window.console.error(t.toStringWithCauses + "\n" + t.stackTraceAsString)
        scope.modState(state ⇒ state.copy(
          isConnected = false,
          ordersState = state.ordersState.copy( // ordersState.error ???
            error = Some(t.toStringWithCauses))))
    }

  def render(props: GuiComponent.Props, state: GuiState): VdomElement =
    new GuiRenderer(props, StateSnapshot(state).setStateVia(scope), toggleFreezed).render

  private def toggleFreezed: Callback =
    for {
      state ← scope.state
      callback ←
        if (state.appState == AppState.Freezed)
          continueRequestingEvents()
        else
          scope.setState(state.copy(
            appState = AppState.Freezed))
    } yield callback

  private def awake(): Callback =
    for {
      state ← scope.state
      callback ←
        if (state.appState == AppState.Standby)
          continueRequestingEvents()
        else
          Callback.empty
    } yield callback

  private def continueRequestingEvents(): Callback =
    for {
      state ← scope.state
      callback ←
        state.ordersState.content match {
          case content: OrdersState.FetchedContent if !isRequestingEvents ⇒
            Callback.log("Continuing requesting events...") >>
              scope.modState(_.copy(
                appState = AppState.RequestingEvents)
              ) >>
              requestAndHandleEvents(after = content.eventId, forStep = state.ordersState.step)
                .delay(10.milliseconds).void   // Without delay, change of appState will not have taken effect in requestAndHandleEvents ???
          case _ ⇒
            Callback.empty
        }
    } yield callback
}

object GuiBackend {
  private val FirstEventTimeout =  0.seconds   // Short timeout to check connection
  private val EventTimeout      = 50.seconds
  private val ContinueDelay     = if (isMobile) 1.second else 500.milliseconds
  private val AfterTimeoutDelay = 1.second
  private val TornDelay         = 1.second

  private def newAfterErrorDelayIterator = (Iterator(1, 2, 4, 6) ++ Iterator.continually(10)) map (_.seconds)

  private val InitialFetchedContext = OrdersState.FetchedContent(Map(), Map(), eventId = EventId.BeforeFirst, eventCount = 0)
}
