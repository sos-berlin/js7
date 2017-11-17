package com.sos.jobscheduler.master.gui.components.gui

import com.sos.jobscheduler.master.gui.components.gui.GuiBackend._
import com.sos.jobscheduler.master.gui.components.state.{GuiState, OrdersState}
import com.sos.jobscheduler.master.gui.data.{Order, OrderEvent}
import com.sos.jobscheduler.master.gui.data.event.{EventId, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.master.gui.services.MasterApi
import com.sos.jobscheduler.master.gui.services.MasterApi.Response
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, Callback, CallbackTo}
import org.scalajs.dom
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class GuiBackend(scope: BackendScope[Unit, GuiState]) {

  private val originalTitle = dom.document.title
  private val onDocumentVisibilityChanged = (_: dom.raw.Event) ⇒ sleep.tryAwake().runNow()
  private var isFetchingEvents = false  // Place in GuiState?

  def componentDidMount() =
    Callback {
      dom.document.addEventListener("visibilitychange", onDocumentVisibilityChanged)
    } >>
    Callback.future {
      for (overviewResponse ← MasterApi.overview) yield
        scope.modState(_.copy(
          isConnected = overviewResponse.isRight,
          overview = Some(overviewResponse))
        ) >>
        fetchOrders()
    }

  def componentWillUnmount() = Callback {
    dom.document.removeEventListener("visibilitychange", onDocumentVisibilityChanged)
  }

  def fetchOrders() = Callback.future {
    for (response ← MasterApi.orders) yield
      response match {
        case Right(stamped: Stamped[Seq[Order[Order.State]]]) ⇒
          for {
            state ← scope.state
            _ ← scope.setState(state.copy(
                isConnected = true,
                ordersState = state.ordersState.updateOrders(stamped)))
            callback ← fetchAndHandleEvents(after = stamped.eventId, forStep = state.ordersState.step + 1)
          } yield callback

        case Left(err) ⇒
          scope.modState(state ⇒ state.copy(
            isConnected = false,
            ordersState = state.ordersState.copy(
              content = InitialFetchedContext,
              error = Some(err),
              step = state.ordersState.step + 1)))
      }
  }

  private def fetchAndHandleEvents(
    after: EventId,
    forStep: Int,
    delay: FiniteDuration = 0.seconds,
    afterErrorDelay: Iterator[FiniteDuration] = newAfterErrorDelayIterator)
  : Callback = {
      def fetchEvents() = Callback.future {
        MasterApi.events(after = after, timeout = if (delay > 0.seconds) FirstEventTimeout else EventTimeout)
          .andThen { case _ ⇒
            isFetchingEvents = false  // TODO Falls fetchOrders() aufgerufen wird, während Events geholt werden, wird isFetchingEvents zu früh zurückgesetzt (wegen doppelter fetchEvents)
          } map
            handleResponse
      }

      def handleResponse(response: Response[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]]): Callback =
        withProperState(forStep) { state ⇒
          val step = state.ordersState.step
          response match {
            case Left(_) ⇒
              scope.setState(state.copy(
                isConnected = false)
              ) >>
                fetchAndHandleEvents(after = after, forStep = step, afterErrorDelay = afterErrorDelay)
                  .delay(afterErrorDelay.next()).map(_ ⇒ ())

            case Right(EventSeq.Empty(lastEventId)) ⇒
              scope.setState(state.copy(
                isConnected = true)
              ) >>
                fetchAndHandleEvents(after = lastEventId, forStep = step, delay = AfterTimeoutDelay)

            case Right(EventSeq.NonEmpty(stampedEvents)) ⇒
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
                fetchAndHandleEvents(after = stampedEvents.last.eventId, forStep = nextStep, delay = ContinueDelay)

            case Right(EventSeq.Torn) ⇒
              dom.console.warn("EventSeq.Torn")
              fetchOrders().delay(TornDelay).map(_ ⇒ ())
          }
      }

      isFetchingEvents = true
      CallbackTo[Callback] {
        if (dom.document.hidden) {
          isFetchingEvents = false
          sleep.sleep()
        } else
          fetchEvents()
      }.flatten.delay(delay) map (_ ⇒ ())
    }

  private def withProperState(forStep: Int)(body: GuiState ⇒ Callback): Callback =
    for {
      state ← scope.state
      callback ←
        if (state.ordersState.step == forStep)
          body(state)
        else {
          //dom.console.log(s"forStep=$forVersion != state.version=${state.version} - Callback discarded")
          Callback.empty
        }
    } yield callback

  def render(state: GuiState): VdomElement =
    new GuiRenderer(state).render

  private object sleep {
    def sleep() = Callback {
      dom.console.log("Sleeping...")
      dom.document.title = originalTitle + SleepSuffix
    }

    def tryAwake(): Callback =
      for {
        state ← scope.state
        ordersState = state.ordersState
        callback ← ordersState.content match {
            case content: OrdersState.FetchedContent if !dom.document.hidden && !isFetchingEvents ⇒
              dom.console.log("Awaking...")
              dom.document.title = originalTitle
              fetchAndHandleEvents(after = content.eventId, forStep = ordersState.step)

            case _ ⇒
              Callback.empty
          }
      } yield callback
  }
}

object GuiBackend {
  private val FirstEventTimeout =  1.second   // Short timeout to check connection
  private val EventTimeout      = 60.seconds
  private val ContinueDelay     =  250.milliseconds
  private val AfterTimeoutDelay = 1000.milliseconds
  private val TornDelay         = 1000.milliseconds
  private val SleepSuffix = "\uD83C\uDF19" // Moon
  // ⚠✝☁☽☾. Official symbol for power sleep mode in UNICODE 9: "\u23FE"

  private def newAfterErrorDelayIterator = (Iterator(1, 2, 4, 6) ++ Iterator.continually(10) ) map (_.seconds)

  private val InitialFetchedContext = OrdersState.FetchedContent(Map(), Nil, eventId = EventId.BeforeFirst, eventCount = 0)
}
