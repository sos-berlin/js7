package com.sos.jobscheduler.master.gui.components.gui

import com.sos.jobscheduler.master.gui.components.gui.GuiBackend._
import com.sos.jobscheduler.master.gui.components.state.{GuiState, OrdersState}
import com.sos.jobscheduler.master.gui.data.Order
import com.sos.jobscheduler.master.gui.data.event.{EventId, EventSeq, Stamped}
import com.sos.jobscheduler.master.gui.services.MasterApi
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, Callback}
import org.scalajs.dom
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class GuiBackend(scope: BackendScope[Unit, GuiState]) {

  private val originalTitle = dom.document.title
  private val onDocumentVisibilityChanged = (_: dom.raw.Event) ⇒ sleep.tryAwake()
  private var isFetchingEvents = false  // Place in GuiState?

  def mount() = Callback.future {
    for (overview ← MasterApi.overview) yield {
      scope.modState(_.copy(overview = Some(overview), isConnected = overview.isRight)) >>
        Callback {
          dom.document.addEventListener("visibilitychange", onDocumentVisibilityChanged)
        } >>
          fetchOrders()
    }
  }

  def unmount() = Callback {
    dom.document.removeEventListener("visibilitychange", onDocumentVisibilityChanged)
  }

  def fetchOrders() = Callback.future {
    for (response ← MasterApi.orders) yield
      response match {
        case Right(stamped: Stamped[Seq[Order[Order.State]]]) ⇒
          for {
            state ← scope.state
            orderList = state.orderState
          } yield {
            val orders = stamped.value
            val newOrderList = orderList.copy(
              content = OrdersState.FetchedContent(
                idToOrder = orders.map(v ⇒ v.id → v).toMap,
                sequence = orders.map(_.id).sorted.reverse.toList,
                eventId = stamped.eventId, eventCount = 0),
              error = None,
              step = orderList.step + 1)
            val newState = state.copy(orderState = newOrderList, isConnected = true)
            (scope.setState(newState) >>
              fetchAndHandleEvents(after = stamped.eventId, forStep = orderList.step + 1))
            .runNow()
          }
        case Left(err) ⇒
          scope.modState(state ⇒ state.copy(
            orderState = state.orderState.copy(
              content = InitialFetchedContext,
              error = Some(err),
              step = state.orderState.step + 1),
            isConnected = false))
      }
  }

  private def fetchAndHandleEvents(
    after: EventId,
    forStep: Int,
    delay: FiniteDuration = 0.seconds,
    afterErrorDelay: Iterator[FiniteDuration] = newAfterErrorDelayIterator)
  : Callback = {
      isFetchingEvents = true
      Callback {
        if (dom.document.hidden) {
          isFetchingEvents = false
          sleep.sleep()
        } else {
          val whenResponded = MasterApi.events(after = after, timeout = if (delay > 0.seconds) FirstEventTimeout else EventTimeout)
            .andThen { case _ ⇒ isFetchingEvents = false  // TODO Falls fetchOrders() aufgerufen wird, während Events geholt werden, wird isFetchingEvents zu früh zurückgesetzt (wegen doppelter fetchEvents)
          }
          for (response ← whenResponded) {
            withProperState(forStep) { state ⇒
              val step = state.orderState.step
              response match {
                case Left(_) ⇒
                  scope.modState { _.copy(isConnected = false) } >>
                    fetchAndHandleEvents(after = after, forStep = step, afterErrorDelay = afterErrorDelay)
                      .delay(afterErrorDelay.next()).map(_ ⇒ ())

                case Right(EventSeq.Empty(lastEventId)) ⇒
                  scope.modState { _.copy(isConnected = true) } >>
                    fetchAndHandleEvents(after = lastEventId, forStep = step, delay = AfterTimeoutDelay)

                case Right(EventSeq.NonEmpty(stampedEvents)) ⇒
                  val nextStep = step + 1
                  scope.setState(
                    state.copy(
                      isConnected = true,
                      orderState = state.orderState.copy(
                        content = state.orderState.content match {
                          case content: OrdersState.FetchedContent ⇒
                            val updated = content.handleEvents(stampedEvents)
                            fetchAndHandleEvents(after = stampedEvents.last.eventId, forStep = nextStep, delay = ContinueDelay)
                              .runNow()
                            updated
                          case o ⇒ o  // Ignore the events
                        },
                        step = nextStep)))

                case Right(EventSeq.Torn) ⇒
                  dom.console.warn("EventSeq.Torn")
                  fetchOrders().delay(TornDelay).map(_ ⇒ ())
              }
            }.runNow()
          }
        }
      }.delay(delay) map (_ ⇒ ())
    }

  private def withProperState(forStep: Int)(body: GuiState ⇒ Callback): Callback =
    for {
      state ← scope.state
      callback ←
        if (state.orderState.step == forStep)
          body(state)
        else {
          //dom.console.log(s"forStep=$forVersion != state.version=${state.version} - Callback discarded")
          Callback.empty
        }
    } yield callback

  def render(state: GuiState): VdomElement =
    new GuiRenderer(state).render

  private object sleep {
    def sleep(): Unit = {
      dom.console.log("Sleeping...")
      dom.document.title = originalTitle + SleepSuffix
    }

    def tryAwake(): Unit =
      ( for {
          state ← scope.state
          orderList = state.orderState
        } yield
          orderList.content match {
            case content: OrdersState.FetchedContent if !dom.document.hidden && !isFetchingEvents ⇒
              dom.console.log("Awaking...")
              dom.document.title = originalTitle
              fetchAndHandleEvents(after = content.eventId, forStep = orderList.step)
                .runNow()

            case _ ⇒
          }
      ).runNow()
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
