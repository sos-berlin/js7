package com.sos.scheduler.engine.common.convert

import com.sos.scheduler.engine.common.convert.Converters.To
import scala.util.control.NonFatal

/**
  * Provides methods for convertion of the result of a PartialFunction (for example a Map).
  *
  * @author Joacim Zschimmer
  */
trait ConvertiblePartialFunction[K, V] {
  this: PartialFunction[K, V] ⇒

  def as[W](key: K)(implicit convert: To[V, W]): W = {
    val value = apply(key)
    convertException(key) {
      convert(value)
    }
  }

  def as[W](key: K, default: ⇒ W)(implicit convert: To[V, W]): W =
    convertException(key) {
      lift(key) map convert.apply getOrElse default
    }

  def optionAs[W](key: K)(implicit convert: To[V, W]): Option[W] =
    convertException(key) {
      lift(key) map convert.apply
    }

  protected def renderKey(key: K) = s"key '$key'"

  private def convertException[W](key: K)(f: ⇒ W): W =
    try f
    catch {
      case NonFatal(t) ⇒ throw new IllegalArgumentException(s"${renderKey(key)}: $t", t)
    }
}
