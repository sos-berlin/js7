package com.sos.jobscheduler.master.gui.common

import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId}
import japgolly.scalajs.react.component.Generic
import japgolly.scalajs.react.vdom.Implicits._
import japgolly.scalajs.react.vdom.{TagMod, VdomArray, VdomNode}
import org.scalajs.dom
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
object Utils {

  val emptyTagMod: TagMod = TagMod.Composite(Vector.empty)
  val emptyVdomNode: VdomNode = vdomNodes()

  lazy val isMobile: Boolean = {
    val u = dom.window.navigator.userAgent
    val is = u matches ".*\bMobile\b.*"
    if (is) dom.console.log(s"isMobile: $u")
    is
  }

  def eventToLog(eventId: EventId, orderId: OrderId, event: OrderEvent) =
    s"${EventId.toString(eventId)} $orderId ${event.getClass.getSimpleName}"

  def vdomNodes(nodes: VdomNode*): VdomArray =
    toVdomArray(nodes)

  def toVdomArray[A](tags: Iterable[A])(implicit f: A ⇒ VdomNode): VdomArray =
    tags.toVdomArray

  def tagMods(tagMods: TagMod*): TagMod =
    TagMod.Composite(tagMods.toVector)

  object ops {
    implicit def maybeToTagMod(o: Option[TagMod]): TagMod =
      o match {
        case Some(x) ⇒ x
        case None ⇒ emptyTagMod
      }

    implicit def maybeUnmountedToTagMod(o: Option[Generic.UnmountedWithRoot[_, _, _, _]]): TagMod =
      o match {
        case Some(x) ⇒ x
        case None ⇒ emptyTagMod
      }
  }
}
