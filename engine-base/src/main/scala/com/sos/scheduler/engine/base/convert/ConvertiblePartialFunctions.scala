package com.sos.scheduler.engine.base.convert

import scala.language.higherKinds
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object ConvertiblePartialFunctions {

  /**
    * Provides methods for conversion of the result of a PartialFunction (for example a Map).
    */
  implicit class ImplicitConvertablePF[K, V](val delegate: PartialFunction[K, V]) extends AnyVal {
    def as[W](key: K)(implicit convert: As[V, W], renderKey: RenderKey): W =
      wrappedConvert(convert, renderKey(key))(delegate(key))

    def as[W](key: K, default: ⇒ W)(implicit convert: As[V, W], renderKey: RenderKey): W =
      optionAs[W](key) getOrElse default

    def optionAs[W](key: K)(implicit convert: As[V, W], renderKey: RenderKey = renderKey): Option[W] =
      delegate.lift(key) map wrappedConvert(convert, renderKey(key))
  }

  /**
    * Calls `convert` and wrap a possible exception with `keyString`.
    */
  def wrappedConvert[K, V, W](convert: V ⇒ W, keyString: ⇒ String): V ⇒ W =
    value ⇒
      try convert(value)
      catch {
        case NonFatal(t) ⇒ throw new IllegalArgumentException(s"Invalid $keyString: $t", t)
      }

  trait RenderKey extends (Any ⇒ String)

  implicit object renderKey extends RenderKey {
    def apply(key: Any) = s"key '$key'"
  }
}
