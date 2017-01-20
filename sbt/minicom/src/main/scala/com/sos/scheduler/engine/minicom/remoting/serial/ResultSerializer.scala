package com.sos.scheduler.engine.minicom.remoting.serial

import akka.util.ByteString
import com.sos.scheduler.engine.minicom.remoting.calls.{CreateInstanceResult, EmptyResult, InvokeResult, MessageClass, Result}
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyRegister
import com.sos.scheduler.engine.minicom.types.HRESULT.S_OK

/**
 * @author Joacim Zschimmer
 */
private final class ResultSerializer(protected val proxyRegister: ProxyRegister) extends IDispatchSerializer {

  def writeResult(result: Result): Unit = {
    writeByte(MessageClass.Answer)
    result match {
      case CreateInstanceResult(invocable) ⇒
        writeInt32(S_OK.value)
        writeInt32(S_OK.value)  // For IID
        writeIUnknown(invocable)
      case InvokeResult(value) ⇒
        writeInt32(S_OK.value)
        writeVariant(value)
      case EmptyResult ⇒
    }
  }
}

private[remoting] object ResultSerializer {
  /**
   * @return (Array, length)
   */
  def serializeResult(proxyRegister: ProxyRegister, result: Result): ByteString = {
    val serializer = new ResultSerializer(proxyRegister)
    serializer.writeResult(result)
    serializer.toByteString
  }
}
