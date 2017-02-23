package com.sos.scheduler.engine.minicom.remoting.serial

import akka.util.ByteString
import com.sos.scheduler.engine.minicom.remoting.calls.MessageClass
import com.sos.scheduler.engine.minicom.types.COMException
import com.sos.scheduler.engine.minicom.types.HRESULT.DISP_E_EXCEPTION

/**
 * @author Joacim Zschimmer
 */
private[remoting] object ErrorSerializer {

  def serializeError(t: Throwable): ByteString = {
    val b = new BaseSerializer
    b.writeByte(MessageClass.Error)
    val code = throwableToHResult(t).comString
    b.writeString("name=COM")
    b.writeString(f"code=$code")
    b.writeString(s"what=$code $t")   // Code prefix COM-xxxxxxxx compatible with C++ agent
    b.toByteString
  }

  private def throwableToHResult(t: Throwable) = t match {
    case t: COMException ⇒ t.hResult
    case _ ⇒ DISP_E_EXCEPTION
  }
}
