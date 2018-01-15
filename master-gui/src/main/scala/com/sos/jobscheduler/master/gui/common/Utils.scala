package com.sos.jobscheduler.master.gui.common

import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId}
import japgolly.scalajs.react.vdom.TagMod
import org.scalajs.dom.window
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions}

/**
  * @author Joacim Zschimmer
  */
object Utils {

  val emptyTagMod: TagMod = TagMod()

  lazy val isMobile: Boolean = {
    val u = window.navigator.userAgent
    val is = u matches ".*\bMobile\b.*"
    if (is) window.console.log(s"isMobile: $u")
    is
  }

  def eventToLog(eventId: EventId, orderId: OrderId, event: OrderEvent) =
    s"${EventId.toString(eventId)} $orderId ${event.getClass.getSimpleName}"

  object ops {
    implicit class WhenTraversableOnce[M[X] <: TraversableOnce[X], A](val underlying: M[A]) extends AnyVal {
      def whenNonEmpty(implicit f: M[A] ⇒ TagMod): TagMod =
        if (underlying.nonEmpty)
          f(underlying)
        else
          emptyTagMod
    }
  }

  def memoize[K, V](f: K ⇒ V): K ⇒ V =
    new mutable.HashMap[K, V] {
      override def apply(key: K) = getOrElseUpdate(key, f(key))
    }
}
