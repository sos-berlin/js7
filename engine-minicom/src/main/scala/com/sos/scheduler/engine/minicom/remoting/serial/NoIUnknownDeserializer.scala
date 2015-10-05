package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.idispatch.Invocable

/**
 * @author Joacim Zschimmer
 */
private[remoting] trait NoIUnknownDeserializer {
  protected def readInvocableOrNull(): Invocable = throw new UnsupportedOperationException("readInvocableOption is not implemented")  // Method is overridden
}
