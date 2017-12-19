package com.sos.jobscheduler.master.gui.common

import com.sos.jobscheduler.base.generic.IsString
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
import japgolly.scalajs.react.vdom.Implicits._
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^.{<, ^}
import scala.language.implicitConversions
import scala.scalajs.js.URIUtils.encodeURI

/**
  * @author Joacim Zschimmer
  */
object Renderers {

  implicit def isStringToTagMod(o: IsString): TagMod =
    o.string

  implicit def optionToTagMod[A](a: Option[A])(implicit toTagMod: A ⇒ TagMod): TagMod =
    a match {
      case None ⇒ "—"
      case Some(o) ⇒ o
    }

  implicit def orderIdToTagMod(orderId: OrderId): TagMod =
    <.a(^.cls := "hidden-link OrderId", ^.href := s"#order/${encodeURI(orderId.string)}")(orderId.string)

  implicit def orderStateToTagMod(state: Order.State): TagMod =
    state match {
      case Order.Scheduled(at)  ⇒ <.span(^.cls := "Order-Symbol-Scheduled") (s"Scheduled for ${at.toReadableLocaleIsoString}")
      case Order.StartNow       ⇒ <.span(^.cls := "Order-Symbol-StartNow")  (state.toString)
      case Order.InProcess      ⇒ <.span(^.cls := "Order-Symbol-InProcess") (state.toString)
      case Order.Ready          ⇒ <.span(^.cls := "Order-Symbol-Ready")     (state.toString)
      case _: Order.Forked      ⇒ <.span(^.cls := "Order-Symbol-Forked")    (state.toString)
      case Order.Processed      ⇒ <.span(^.cls := "Order-Symbol-Processed") (state.toString)
      case Order.Finished       ⇒ <.span(^.cls := "Order-Symbol-Finished")  (state.toString)
      case _                    ⇒ state.toString
    }

  object forTable {
    implicit def orderStateToTagMod(state: Order.State): TagMod =
      state match {
        case Order.Scheduled(at) ⇒ <.span(^.cls := "Order-Symbol-Scheduled")(at.toReadableLocaleIsoString)
        case o ⇒ Renderers.orderStateToTagMod(o)
      }
  }

  implicit def orderAttachedToTagMod(attachedTo: Order.AttachedTo): TagMod =
    attachedTo match {
      case Order.AttachedTo.Agent(agentPath) ⇒ agentPath.string
      case Order.AttachedTo.Detachable(agentPath) ⇒ <.span(^.cls := "AttachedTo-Detachable")(agentPath.string)
      case _ ⇒ attachedTo.toString
    }

  implicit def outcomeToTagMod(outcome: Outcome): TagMod =
    outcome match {
      case Outcome.Good(true) ⇒ "🔅 true"
      case Outcome.Good(false) ⇒ "☁ ️false"
      case Outcome.Bad(_) ⇒ s"💥 $outcome"
      case _ ⇒ outcome.toString
    }
}
