package com.sos.scheduler.engine.base.exceptions

/**
 * @author Joacim Zschimmer
 */
class StandardPublicException(val publicMessage: String, cause: Throwable = null)
extends RuntimeException(publicMessage, cause)
with PublicException {

  def this(publicMessage: String) = this(publicMessage, null: Throwable)
}
