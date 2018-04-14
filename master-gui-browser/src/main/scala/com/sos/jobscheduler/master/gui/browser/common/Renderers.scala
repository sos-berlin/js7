package com.sos.jobscheduler.master.gui.browser.common

import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.data.filebased.TypedPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.{WorkflowId, WorkflowPath}
import com.sos.jobscheduler.master.gui.browser.router.Router
import japgolly.scalajs.react.vdom.Implicits._
import japgolly.scalajs.react.vdom.html_<^.{<, ^}
import japgolly.scalajs.react.vdom.{VdomArray, VdomNode}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object Renderers {

  implicit def genericStringToVdom(o: GenericString): VdomNode =
    o.toString

  implicit def typedPathToVdom(o: TypedPath): VdomNode =
    VdomArray(o.companion.camelName, " ", o.string)

  implicit def workflowIdToVdom(id: WorkflowId): VdomNode =
    <.a(^.cls := "hidden-link", ^.href := Router.hash(id), id.pretty)

  implicit def workflowPathToVdom(o: WorkflowPath): VdomNode =
    <.a(^.cls := "hidden-link", ^.href := Router.hash(o), o.pretty)

  implicit def orderIdToVdom(orderId: OrderId): VdomNode =
    <.a(^.cls := "hidden-link OrderId", ^.href := Router.hash(orderId))(orderId.string)

  implicit final class OptionalVdom[A](private val underlying: Option[A]) extends AnyVal {
    def orMissing(implicit toVdom: A ⇒ VdomNode): VdomNode =
      underlying match {
        case None ⇒ "—"
        case Some(o) ⇒ o
      }
  }

  implicit def orderStateToVdom(state: Order.State): VdomNode =
    VdomArray(
      orderStateToSymbolFixedWidth(state),
      orderStateTextToVdom(state))

  object forTable {
    implicit def orderStateToVdom(state: Order.State): VdomNode =
      VdomArray(
        orderStateToSymbolFixedWidth(state),
        state match {
           case Order.Fresh(Some(at)) ⇒ at.toReadableLocaleIsoString: VdomNode
           case _ ⇒ orderStateTextToVdom(state)
        })
  }

  private def orderStateTextToVdom(state: Order.State): VdomNode =
    state match {
      case Order.Fresh(Some(at))  ⇒ s"Scheduled for ${at.toReadableLocaleIsoString}"
      case Order.Processed(Outcome.Succeeded(ReturnCode.Success)) ⇒ s"Processed"
      case Order.Processed(o: Outcome.Undisrupted) ⇒ s"Processed rc=${o.returnCode.number}"
      case Order.Processed(o: Outcome.Disrupted) ⇒ s"Processed $o"
      case Order.Join(children) ⇒ s"Join ${children.size}×"
      case _ ⇒ state.toString
    }

  def orderStateToSymbolFixedWidth(state: Order.State): VdomNode = {
    val symbol = orderStateToSymbol(state)
    //if (symbol.isInstanceOf[VdomElement])  // material-icons
    //  VdomArray(symbol, " ")
    //else
      <.span(^.cls := "Order-State-symbol-width")(symbol)
  }

  def orderStateToSymbol(state: Order.State): VdomNode =
    state match {
      case Order.Fresh(Some(_)) ⇒ <.i(^.cls := "material-icons text-prefix", "access_alarm")
      case Order.Fresh(None)  ⇒ "━"
      case Order.InProcess    ⇒ <.i(^.cls := "material-icons text-prefix rotate-slowly gear", "settings")
      case _: Order.Join      ⇒ "⨁"
      case Order.Processed(_: Outcome.Succeeded) ⇒ <.i(^.cls := "material-icons text-prefix sunny")("wb_sunny") // "🔅"  "⬇"
      case Order.Processed(_: Outcome.Failed) ⇒ <.i(^.cls := "material-icons text-prefix")("wb_cloudy") // "☁"
      case Order.Processed(_: Outcome.Disrupted) ⇒ "💥" // "⬇"
      case Order.Ready        ⇒ "━"
      case Order.Finished     ⇒ "☆"
      case _                  ⇒ "·"
    }

  implicit def orderAttachedToVdom(attachedTo: Order.AttachedTo): VdomNode =
    attachedTo match {
      case Order.AttachedTo.Agent(agentId) ⇒ agentId.toSimpleString
      case Order.AttachedTo.Detachable(agentId) ⇒ <.span(^.cls := "AttachedTo-Detachable")(agentId.toSimpleString)
      case _ ⇒ attachedTo.toString
    }

  implicit def outcomeToVdom(outcome: Outcome): VdomNode =
    VdomArray(outcomeSymbol(outcome), " ", outcome.toString)

  def outcomeSymbol(outcome: Outcome): VdomNode =
    outcome match {
      case Outcome.Succeeded(_)  ⇒ <.i(^.cls := "material-icons text-prefix sunny")("wb_sunny")   // "🔅"
      //case ReturnCode(ReturnCode(1)) ⇒ <.i(^.cls := "material-icons text-prefix")("wb_cloudy")  // "☁"
      case Outcome.Disrupted(_) ⇒ "💥"
      case _ ⇒ ""
    }
}
