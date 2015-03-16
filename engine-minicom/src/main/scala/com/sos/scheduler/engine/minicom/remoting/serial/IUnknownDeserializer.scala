package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.types.CLSID
import javax.annotation.Nullable
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
private[remoting] trait IUnknownDeserializer extends VariantDeserializer {

  protected val remoting: ServerRemoting

  @Nullable
  override final def readInvocableOrNull() = {
    val proxyId = ProxyId(readInt64())
    val isNew = readBoolean()
    if (isNew) {
      val name = readString()
      val proxyClsid = CLSID(readUUID())
      val proxyProperties = immutable.Seq.fill(readInt32()) {
        val name = readString()
        val value = readVariant()
        name → value
      }
      remoting.newProxy(proxyId, name, proxyClsid, proxyProperties)
    }
    else
      proxyId match {
        case ProxyId.Null ⇒ null
        case _ ⇒ remoting.invocable(proxyId)
      }
  }
}
