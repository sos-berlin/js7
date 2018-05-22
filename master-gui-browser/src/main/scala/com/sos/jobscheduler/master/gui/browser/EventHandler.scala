package com.sos.jobscheduler.master.gui.browser

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.data.event.{EventId, Stamped}
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.gui.browser.EventHandler._
import com.sos.jobscheduler.master.gui.browser.GuiRenderer.Moon
import com.sos.jobscheduler.master.gui.browser.common.Utils.isMobile
import com.sos.jobscheduler.master.gui.browser.components.state.{AppState, GuiState, OrdersState, PreparedWorkflow}
import com.sos.jobscheduler.master.gui.browser.services.MasterApi
import japgolly.scalajs.react.{BackendScope, Callback, CallbackTo}
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom.window
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
trait EventHandler {
  protected def scope: BackendScope[GuiComponent.Props, GuiState]

  protected var _isRequestingEvents = false
  def isRequestingEvents = _isRequestingEvents

  protected def fetchAndHandleEvents(
    after: EventId,
    forStep: Int,
    timeout: FiniteDuration = EventTimeout,
    afterErrorDelay: Iterator[FiniteDuration])
  : Callback

  final def startRequestAndHandleEvents(
    after: EventId,
    forStep: Int,
    timeout: FiniteDuration = EventTimeout,
    afterErrorDelay: Iterator[FiniteDuration] = newAfterErrorDelayIterator)
  : Callback =
      scope.state.flatMap(state ⇒
        ifInactive(state) getOrElse
          fetchAndHandleEvents(after, forStep, timeout, afterErrorDelay))
      .attemptTry flatMap {
        case Success(()) ⇒ Callback.empty
        case Failure(t) ⇒ onGuiFailed(t)
      }

  protected final def ifInactive(state: GuiState): Option[Callback] =
    if (state.appState != AppState.RequestingEvents)
      Some(Callback.empty)
    else if (window.document.hidden) {
      window.console.log(s"$Moon Standby...")
      Some(scope.modState(_.copy(
        appState = AppState.Standby)))
    } else None

  final def requestStateAndEvents: Callback =
    for {
      _ ← requestWorkflows
      state ← scope.state
      callback ← requestOrdersAndEvents2.when(state.appState == AppState.RequestingEvents).void
    } yield callback

  private def requestOrdersAndEvents2: Callback =
    scope.modState(o ⇒ o.copy(
      ordersState = o.ordersState.copy(
        content = OrdersState.FetchingContent))
    ) >>
    Callback.future {
      MasterApi.orders.runAsync transform {
        case Failure(err) ⇒
          onRequestInitialStateError(err).runNow()
          Failure(err)
        case Success(stamped: Stamped[Seq[Order[Order.State]]]) ⇒
          Try {
            for {
              state ← scope.state
              _ ← scope.setState(state.copy(
                  isConnected = true,
                  ordersState = state.ordersState.updateOrders(stamped)))
              callback ← startRequestAndHandleEvents(after = stamped.eventId, forStep = state.ordersState.step + 1)
            } yield callback
          }
      }
    }

  private def requestWorkflows: Callback =
    Callback.future {
      MasterApi.workflows.runAsync transform {
        case Failure(err) ⇒
          onRequestInitialStateError(err).runNow()
          Failure(err)
        case Success(stamped: Stamped[Seq[Workflow]]) ⇒
          Try {
            scope.modState(_.copy(
              idToWorkflow = stamped.value.map(o ⇒ o.id → PreparedWorkflow(o.id, o)).toMap))
          }
      }
    }

  private def onRequestInitialStateError(throwable: Throwable): Callback =
    onGuiFailed(throwable) >>
    scope.modState(state ⇒ state.copy(
      ordersState = state.ordersState.copy(
        content = InitialFetchedContext)))

  protected final def onGuiFailed(throwable: Throwable): CallbackTo[Unit] = {
    window.console.error(throwable.toStringWithCausesAndStackTrace)
    scope.modState(o ⇒ o.copy(
      isConnected = false,
      ordersState = o.ordersState.copy(
        error = Some(throwable.toStringWithCauses)))
    ) //>>
      //(throwable match {
      //  case _ if unmounted ⇒
      //    Callback.empty
      //  case t: HttpClientException if t.reason.isUnreachable ⇒
      //    window.console.info("isUnreachable")
      //    window.console.warn(t.toStringWithCauses)
      //    if (window.document.hidden)    // Stops when browser windows is hidden and does not restart
      //      Callback.empty
      //    else
      //      start().delay(UnreachableDelay).map(_ ⇒ ())
      //  case _ ⇒
      //    window.console.info("!isUnreachable")
      //    onGuiFailed(throwable)
      //  })
  }
}

object EventHandler {
  val UnreachableDelay  =  5.seconds
  val FirstEventTimeout =  0.seconds   // Short timeout to check connection
  val EventTimeout      = 50.seconds
  val ContinueDelay     = if (isMobile) 1.second else 500.milliseconds
  val AfterTimeoutDelay = 1.second
  val TornDelay         = 1.second
  private val InitialFetchedContext = OrdersState.FetchedContent(Map(), Map(), eventId = EventId.BeforeFirst, eventCount = 0)

  def newAfterErrorDelayIterator = (Iterator(1, 2, 4, 6) ++ Iterator.continually(10)) map (_.seconds)
}
