package com.sos.jobscheduler.master.gui.common

import com.sos.jobscheduler.base.generic.IsString
import com.sos.jobscheduler.data.order.Order
import japgolly.scalajs.react.vdom.Implicits._
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^.{<, ^}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object RenderUtils {

  implicit def isStringToTagMod(o: IsString): TagMod =
    o.string

  implicit def toTagMod[A: ToTagMod](a: A): TagMod =
    implicitly[ToTagMod[A]].toTagMod(a)

  implicit val OrderAttachedToToHtml = ToTagMod.toTagMod[Option[Order.AttachedTo]] {
    case None ⇒ "—"
    case Some(Order.AttachedTo.Agent(agentPath)) ⇒ agentPath.toString
    case Some(Order.AttachedTo.Detachable(agentPath)) ⇒ <.span(^.cls := "AttachedTo-Detachable")(agentPath.toString)
  }
}
