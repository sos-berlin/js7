package com.sos.scheduler.engine.minicom.types

/**
 * A COM object.
 * @author Joacim Zschimmer
 */
trait IUnknown {

  /** ProxyRegister needs reference equality */
  final override def equals(o: Any) = AnyRef.equals(o)

  /** ProxyRegister needs reference equality */
  final override def hashCode = AnyRef.hashCode
}
