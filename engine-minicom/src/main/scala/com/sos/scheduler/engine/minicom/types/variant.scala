package com.sos.scheduler.engine.minicom.types

import scala.runtime.BoxedUnit

/**
 * A COM `VARIANT` is represented by Any.
 * @author Joacim Zschimmer
 */
object variant {
  /**
   * A `VT_EMPTY` value is represented by boxed [[Unit]].
   */
  def isEmpty(o: Any) = o == BoxedUnit.UNIT
}
