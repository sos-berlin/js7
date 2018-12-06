package com.sos.jobscheduler.master.gui.browser.common

import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.data.filebased.TypedPath
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

  def orderStateToVdom(order: Order[Order.State]): VdomNode =
    VdomArray(
      orderStateToSymbolFixedWidth(order),
      orderStateTextToVdom(order))

  object forTable {
    def orderStateToVdom(order: Order[Order.State]): VdomNode =
      VdomArray(
        orderStateToSymbolFixedWidth(order),
        order.state match {
           case Order.Fresh(Some(at)) ⇒ at.toReadableLocaleIsoString: VdomNode
           case _ ⇒ orderStateTextToVdom(order)
        })
  }

  private def orderStateTextToVdom(order: Order[Order.State]): VdomNode =
    order.state match {
      case Order.Fresh(Some(at))  ⇒ s"Scheduled for ${at.toReadableLocaleIsoString}"
      case Order.Processed ⇒ outcomeToSymbol(order.outcome)
      case Order.Forked(children) ⇒ s"Forked ${children.size}×"
      case _ ⇒ order.state.toString
    }

  def orderStateToSymbolFixedWidth(order: Order[Order.State]): VdomNode =
    <.span(^.cls := "Order-State-symbol-width")(orderStateToSymbol(order))

  def orderStateToSymbol(order: Order[Order.State]): VdomNode =
    order.state match {
      case Order.Fresh(Some(_)) ⇒ <.i(^.cls := "material-icons text-prefix", "access_alarm")
      case Order.Fresh(None)    ⇒ "━"
      case Order.Processing     ⇒ <.i(^.cls := "material-icons text-prefix rotate-slowly gear", "settings")
      case _: Order.Forked      ⇒ "⨁"
      case Order.Processed      ⇒ outcomeToSymbol(order.outcome)
      case Order.Ready          ⇒ "◯"  // Circle
      case Order.Finished       ⇒ "☆"  // Star
      case _: Order.Stopped     ⇒ "❗"  // Red exclamation mark
      case _: Order.Broken      ⇒ "💥"  // Explosion
      case _                    ⇒ "·"   // Dot
    }

  def outcomeToSymbol(outcome: Outcome): VdomNode =
    outcome match {
      case _: Outcome.Succeeded ⇒ <.i(^.cls := "material-icons text-prefix sunny")("wb_sunny") // "🔅"
      case _: Outcome.Failed ⇒ <.i(^.cls := "material-icons text-prefix")("wb_cloudy") // "☁"
      case _: Outcome.Disrupted ⇒ "💥"  // Explosion
    }

  implicit def orderAttachedStateToVdom(attachedState: Order.AttachedState): VdomNode =
    attachedState match {
      case Order.Attaching(agentPath) ⇒ <.span(^.cls := "AttachedState-Attaching")('(', agentPath.string, ')')
      case Order.Attached(agentId) ⇒ agentId.toSimpleString
      case Order.Detaching(agentId) ⇒ <.span(^.cls := "AttachedState-Detaching")('(', agentId.toSimpleString, ')')
      case _ ⇒ attachedState.toString
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
