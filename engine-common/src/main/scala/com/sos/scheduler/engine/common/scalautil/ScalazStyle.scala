package com.sos.scheduler.engine.common.scalautil

/**
 * Methods like in Scalaz.
 *
 * @author Joacim Zschimmer
 *
 * @see https://github.com/scalaz/scalaz
 */
object ScalazStyle {
  implicit class OptionRichBoolean(val delegate: Boolean) extends AnyVal {

    /**
     * Conditional `Option`.
     * <p>
     * `true.option(a)` == `Some(a)`, `false.option(a)` == `None`
     */
    final def option[A](a: ⇒ A): Option[A] = if (delegate) Some(a) else None
  }
}
