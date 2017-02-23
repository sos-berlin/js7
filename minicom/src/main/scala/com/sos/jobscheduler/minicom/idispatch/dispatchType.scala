package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.minicom.types.COMException
import com.sos.scheduler.engine.minicom.types.HRESULT.E_INVALIDARG
import scala.collection.{immutable, mutable}

/**
 * Type of call using a [[DISPID]].
 */
sealed abstract class DispatchType(val value: Int) {
  override def toString = getClass.getSimpleName stripSuffix "$"   // Scala object class name ends with '$'
}

object DispatchType {
  private[minicom] val values = Vector(DISPATCH_METHOD, DISPATCH_PROPERTYGET, DISPATCH_PROPERTYPUT, DISPATCH_PROPERTYPUTREF)

  def set(bits: Int): immutable.Set[DispatchType] = {
    if ((bits & ~0xF) != 0) throw new COMException(E_INVALIDARG, f"Invalid DispatchType $bits%08x")
    val result = new mutable.ArrayBuffer[DispatchType](values.size)
    for (dispatchType ← values) {
      if ((bits & dispatchType.value) != 0) result += dispatchType
    }
    result.toSet
  }
}

object DISPATCH_METHOD extends DispatchType(1)
object DISPATCH_PROPERTYGET extends DispatchType(2)
object DISPATCH_PROPERTYPUT extends DispatchType(4)
object DISPATCH_PROPERTYPUTREF extends DispatchType(8)

/**
 * COM Dispatch ID, refers to a name for a method or a property.
 * @see [[DispatchType]]
 */
final case class DISPID(value: Int)

object DISPID {
  val PROPERTYPUT = DISPID(-3)
}
