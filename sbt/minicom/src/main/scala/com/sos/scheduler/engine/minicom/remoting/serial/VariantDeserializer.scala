package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.remoting.serial.variantArrayFlags._
import com.sos.scheduler.engine.minicom.remoting.serial.variantTypes._
import com.sos.scheduler.engine.minicom.types.HRESULT.DISP_E_BADVARTYPE
import com.sos.scheduler.engine.minicom.types.{COMException, IUnknown, VariantArray}
import org.scalactic.Requirements._

/**
 * @author Joacim Zschimmer
 */
private[remoting] trait VariantDeserializer extends BaseDeserializer {

  def readVariant(): Any = {
    val vt = readInt32()
    vt match {
      case _ if (vt & VT_ARRAY) != 0 ⇒ readVariantArray()
      case VT_UNKNOWN | VT_DISPATCH ⇒ readIUnknownOrNull()  // To make any sense, VT_UNKNOWN should denote here an IDispatch
      case _ ⇒ readSimpleVariant(vt)
    }
  }

  private def readVariantArray(): VariantArray = {
    val dimensions = readInt16()
    require(dimensions == 1)
    val features = readInt16()
    require((features & ~(FADF_FIXEDSIZE | FADF_HAVEVARTYPE | FADF_BSTR | FADF_VARIANT)) == 0, f"Only FADF_FIXEDSIZE, FADF_HAVEVARTYPE and FADF_VARIANT are accepted, not fFeature=$features%04x")
    // Nicht bei com.cxx (Unix): require((features & FADF_HAVEVARTYPE) != 0, f"FADF_HAVEVARTYPE is required, not fFeature=$features%04x")
    val count = readInt32()
    val lowerBound = readInt32()
    require(lowerBound == 0)
    readInt32() match {
      case VT_VARIANT ⇒
        require((features & FADF_VARIANT) != 0, f"VT_VARIANT requires FADF_HAVEVARTYPE | FADF_VARIANT, not fFeature=$features%04x")
        VariantArray(Vector.fill(count) { readVariant() })
      case o ⇒ throw new COMException(DISP_E_BADVARTYPE, f"Unsupported Array Variant VT=$o%x")
    }
  }

  private def readSimpleVariant(vt: Int): Any =
    vt match {
      case VT_EMPTY ⇒ ()
      case VT_I4 | VT_INT ⇒ readInt32()
      case VT_BSTR ⇒ readString()
      case VT_BOOL ⇒ readBoolean()
      case VT_I8 ⇒ readInt64()
      case VT_ERROR ⇒ throw new COMException(DISP_E_BADVARTYPE, f"Unsupported Variant VT_ERROR")
      case o ⇒ throw new COMException(DISP_E_BADVARTYPE, f"Unsupported Variant VT=$o%02x")
    }

    protected def readIUnknownOrNull(): IUnknown
}
