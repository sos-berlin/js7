package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.idispatch.Invocable
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyRegister
import com.sos.scheduler.engine.minicom.types.CLSID

/**
 * @author Joacim Zschimmer
 */
private[remoting] abstract class IDispatchSerializer
extends VariantSerializer {

  protected val proxyRegister: ProxyRegister

  override final def writeInvocable(invocable: Invocable): Unit = {
    val (proxyId, isNew) = proxyRegister.invocableToProxyId(invocable)
    writeInt64(proxyId.value)
    writeBoolean(isNew)
    if (isNew) {
      writeString(invocable.getClass.getSimpleName)
      writeUUID(CLSID.Null.uuid)
      writeInt32(0)
//      writeUUID(localProxy.clsid.uuid)
//      writeInt32(localProxy.properties.size)
//      for ((name, v) ← localProxy.properties) {
//        writeString(name)
//        writeVariant(v)
//      }
    }
  }
}
