package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.types.IUnknown

/**
 * @author Joacim Zschimmer
 */
private[remoting] trait NoIUnknownDeserializer {
  protected def readIUnknownOrNull(): IUnknown = throw new UnsupportedOperationException("readIUnknownOption is not implemented")  // Method is overridden
}
