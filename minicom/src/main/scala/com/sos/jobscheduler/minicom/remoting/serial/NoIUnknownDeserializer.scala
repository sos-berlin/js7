package com.sos.jobscheduler.minicom.remoting.serial

import com.sos.jobscheduler.minicom.types.IUnknown

/**
 * @author Joacim Zschimmer
 */
private[remoting] trait NoIUnknownDeserializer {
  protected def readIUnknownOrNull(): IUnknown = throw new UnsupportedOperationException("readIUnknownOption is not implemented")  // Method is overridden
}
