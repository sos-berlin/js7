package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.idispatch.Invocable
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.serial.variantTypes._
import scala.runtime.BoxedUnit.UNIT

/**
 * @author Joacim Zschimmer
 */
private[remoting] abstract class VariantSerializer extends BaseSerializer {

  final def writeVariant(value: Any): Unit =
    value match {
      case o: Int ⇒
        writeInt32(VT_I4)
        writeInt32(o)
      case o: Long ⇒
        writeInt32(VT_I8)
        writeInt64(o)
      case o: Boolean ⇒
        writeInt32(VT_BOOL)
        writeBoolean(o)
      case o: String ⇒
        writeInt32(VT_BSTR)
        writeString(o)
      case o: Invocable ⇒
        writeInt32(VT_DISPATCH)
        writeInvocable(o)
      case null ⇒
        writeNull()
      case Unit | UNIT ⇒ writeInt32(VT_EMPTY)
    }

  def writeNull(): Unit = {
    writeInt32(VT_UNKNOWN)
    writeInt64(ProxyId.Null.value)
    writeBoolean(false)
  }

  def writeInvocable(invocable: Invocable): Unit
}

object VariantSerializer {
  final class WithoutIUnknown extends VariantSerializer {
    def writeInvocable(invocable: Invocable) = throw new UnsupportedOperationException("Serialization of IUnknown is not supported")
  }
}
