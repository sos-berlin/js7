package com.sos.jobscheduler.master.gui.common

import com.sos.jobscheduler.base.generic.IsString
import com.sos.jobscheduler.data.order.{Order, Outcome}
import japgolly.scalajs.react.vdom.Implicits._
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^.{<, ^}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object Renderers {

  implicit def isStringToTagMod(o: IsString): TagMod =
    o.string

  implicit def toTagMod[A: ToTagMod](a: A): TagMod =
    implicitly[ToTagMod[A]].toTagMod(a)

  implicit val OrderStateVdom = ToTagMod.toTagMod[Order.State] {
    case Order.Scheduled(at) ⇒ Array[TagMod](
      <.span(^.cls := "hide-on-phone")("Scheduled for "),
      at.toReadableLocaleIsoString).toTagMod

    case o ⇒ o.toString
  }

  implicit val OrderStateAttachedToToTagMod = ToTagMod.toTagMod[Option[Order.AttachedTo]] {
    case None ⇒ "—"
    case Some(Order.AttachedTo.Agent(agentPath)) ⇒ agentPath.string
    case Some(Order.AttachedTo.Detachable(agentPath)) ⇒ <.span(^.cls := "AttachedTo-Detachable")(agentPath.string)
  }

  implicit val OutcomeVdom = ToTagMod.toTagMod[Outcome](_.toString)
}
