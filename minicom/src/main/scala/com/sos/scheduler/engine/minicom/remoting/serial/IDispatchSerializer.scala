package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.remoting.proxy.{HasProxyMeta, ProxyRegister}
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
      iUnknown match {
        case hasProxyMeta: HasProxyMeta ⇒
          val meta = hasProxyMeta.proxyMeta
          writeUUID(meta.clsid.uuid)
          writeInt32(meta.properties.size)
          for ((name, v) ← meta.properties) {
            writeString(name)
            writeVariant(v)
          }
        case _ ⇒
          writeUUID(CLSID.Null.uuid)
          writeInt32(0)
      }
    }
  }
}
