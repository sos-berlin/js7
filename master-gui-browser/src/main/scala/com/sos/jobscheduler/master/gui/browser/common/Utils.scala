package com.sos.jobscheduler.master.gui.browser.common

import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId}
import japgolly.scalajs.react.vdom.TagMod
import org.scalajs.dom.window
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions}
import scala.scalajs.js.URIUtils.encodeURIComponent

/**
  * @author Joacim Zschimmer
  */
object Utils {

  lazy val isMobile: Boolean = {
    val u = window.navigator.userAgent
    val is = u matches ".*\bMobile\b.*"
    if (is) window.console.log(s"isMobile: $u")
    is
  }

  def eventToLog(eventId: EventId, orderId: OrderId, event: OrderEvent) =
    s"${EventId.toString(eventId)} $orderId ${event.getClass.getSimpleName}"

  object ops {
    implicit final class WhenTraversableOnce[M[X] <: TraversableOnce[X], A](private val underlying: M[A]) extends AnyVal {
      def whenNonEmpty(implicit f: M[A] ⇒ TagMod): TagMod =
        if (underlying.nonEmpty)
          f(underlying)
        else
          TagMod.empty
    }
  }

  def memoize[K, V](f: K ⇒ V): K ⇒ V =
    new mutable.HashMap[K, V] {
      override def apply(key: K) = getOrElseUpdate(key, f(key))
    }

  def toUriQueryString(keyValues: Iterable[(String, String)]): String =
    keyValues.map { case (k, v) ⇒
      val kk = encodeURIComponent(k)
      val vv = encodeURIComponent(v)
      s"$kk=$vv"
    }.mkString("&")


  /** For text content, not for attributes. */
  def stringToHtml(string: String) =
    string.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
}
