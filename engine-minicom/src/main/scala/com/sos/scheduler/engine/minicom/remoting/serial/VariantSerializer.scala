package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.idispatch.IDispatch
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.invoker.IDispatchInvoker
import com.sos.scheduler.engine.minicom.remoting.serial.variantArrayFlags.{FADF_HAVEVARTYPE, FADF_VARIANT}
import com.sos.scheduler.engine.minicom.remoting.serial.variantTypes._
import com.sos.scheduler.engine.minicom.types.{IUnknown, VariantArray}
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
      case o: Double ⇒
        writeInt32(VT_R8)
        writeDouble(o)
      case o: Boolean ⇒
        writeInt32(VT_BOOL)
        writeBoolean(o)
      case o: String ⇒
        writeInt32(VT_BSTR)
        writeString(o)
      case o: sos.spooler.Idispatch ⇒
        writeInt32(VT_DISPATCH)
        writeIUnknown(o.com_invoker.asInstanceOf[IDispatchInvoker].iDispatch)
      case o: IDispatch ⇒
        writeInt32(VT_DISPATCH)
        writeIUnknown(o)
      case null ⇒
        writeNull()
      case Unit | UNIT ⇒
        writeInt32(VT_EMPTY)
      case seq: Seq[_] ⇒
        writeSeq(seq)
      case array: Array[_] ⇒
        writeSeq(array)
      case VariantArray(seq) ⇒  // For compatibility test with VariantDeserializer
        writeSeq(seq)
      case o ⇒
        throw new IllegalArgumentException(s"Not serializable as a COM VARIANT: ${o.getClass.getName}")
    }

  def writeNull(): Unit = {
    writeInt32(VT_UNKNOWN)
    writeInt64(ProxyId.Null.value)
    writeBoolean(false)
  }

  private def writeSeq(seq: Seq[_]): Unit = {
    writeInt32(VT_ARRAY | VT_VARIANT)
    writeInt16(1)  // Dimensions
    writeInt16((FADF_HAVEVARTYPE | FADF_VARIANT).toShort)
    writeInt32(seq.size)
    writeInt32(0)  // Lower bound
    writeInt32(VT_VARIANT)
    seq foreach writeVariant
  }

  def writeIUnknown(iUnknown: IUnknown): Unit
}

object VariantSerializer {
  final class WithoutIUnknown extends VariantSerializer {
    def writeIUnknown(iUnknown: IUnknown) = throw new UnsupportedOperationException("Serialization of IUnknown is not supported")
  }
}
