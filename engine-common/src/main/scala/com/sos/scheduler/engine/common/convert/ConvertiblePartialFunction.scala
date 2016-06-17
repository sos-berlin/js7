package com.sos.scheduler.engine.common.convert

import com.sos.scheduler.engine.common.convert.Converters.To
import com.sos.scheduler.engine.common.convert.ConvertiblePartialFunctions.wrappedConvert

/**
  * Provides methods for convertion of the result of a PartialFunction (for example a Map).
  *
  * @author Joacim Zschimmer
  */
trait ConvertiblePartialFunction[K, V] extends PartialFunction[K, V] {

  def as[W](key: K)(implicit convert: To[V, W]): W =
    wrappedConvert(convert, renderKey(key))(apply(key))

  def as[W](key: K, default: ⇒ W)(implicit convert: To[V, W]): W =
    optionAs[W](key) getOrElse default

  def optionAs[W](key: K)(implicit convert: To[V, W]): Option[W] =
    lift(key) map wrappedConvert(convert, renderKey(key))

  protected def renderKey(key: K) = s"key '$key'"
}
