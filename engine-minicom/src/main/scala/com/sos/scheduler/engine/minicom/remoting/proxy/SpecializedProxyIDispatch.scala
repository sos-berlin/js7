package com.sos.scheduler.engine.minicom.remoting.proxy

import java.util.NoSuchElementException

/**
 * @author Joacim Zschimmer
 */
trait SpecializedProxyIDispatch extends ProxyIDispatch

object SpecializedProxyIDispatch  {

  /**
   * For simple reading of proxy properties in proxy classes.
   */
  def forEachProperty(keyValues: Iterable[(String, Any)], className: String)(handleProperty: PartialFunction[(String, Any), Unit]): Unit =
    for (kv ← keyValues) handleProperty.applyOrElse(kv, { o: (String, Any) ⇒ throwNoSuchProperty(name = o._1, className = className) })

  /** Like C++ error Z-REMOTE-126 */
  private def throwNoSuchProperty(name: String, className: String) = throw new NoSuchElementException(s"Unknown local proxy property '$name' for $className")

  def requireNoProperties(keyValues: Iterable[(String, Any)], className: String): Unit =
    require(keyValues.isEmpty, s"Unknown local proxy properties: ${keyValues map { _._1 } mkString ","} for $className")
}
