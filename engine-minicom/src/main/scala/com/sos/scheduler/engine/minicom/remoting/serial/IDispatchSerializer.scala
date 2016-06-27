package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyRegister
import com.sos.scheduler.engine.minicom.types.{CLSID, IUnknown}

/**
 * @author Joacim Zschimmer
 */
private[remoting] abstract class IDispatchSerializer
extends VariantSerializer {

  protected val proxyRegister: ProxyRegister

  override final def writeIUnknown(iUnknown: IUnknown): Unit = {
    val (proxyId, isNew) = proxyRegister.iUnknownToProxyId(iUnknown)
    writeInt64(proxyId.value)
    writeBoolean(isNew)
    if (isNew) {
      writeString(iUnknown.getClass.getSimpleName)
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
