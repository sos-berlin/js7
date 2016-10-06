package com.sos.scheduler.engine.base.convert

import com.sos.scheduler.engine.base.convert.ConvertiblePartialFunctions.wrappedConvert

/**
  * Provides methods for convertion of the result of a PartialFunction (for example a Map).
  *
  * @author Joacim Zschimmer
  */
trait ConvertiblePartialFunction[K, V] extends PartialFunction[K, V] {

  def as[W](key: K)(implicit convert: As[V, W]): W =
    wrappedConvert(convert, renderKey(key))(apply(key))

  def as[W](key: K, default: ⇒ W)(implicit convert: As[V, W]): W =
    optionAs[W](key) getOrElse default

  def optionAs[W](key: K, default: ⇒ Option[W])(implicit convert: As[V, W]): Option[W] =
    optionAs(key)(convert) orElse default

  def optionAs[W](key: K)(implicit convert: As[V, W]): Option[W] =
    lift(key) map wrappedConvert(convert, renderKey(key))

  protected def renderKey(key: K) = s"key '$key'"
}
