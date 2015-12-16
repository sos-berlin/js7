package com.sos.scheduler.engine.minicom.remoting.proxy

import java.util.NoSuchElementException

/**
 * @author Joacim Zschimmer
 */
trait SpecializedProxyIDispatch extends CachingProxyIDispatch

object SpecializedProxyIDispatch  {

  /**
   * For simple reading of proxy properties in proxy classes.
   */
  def forEachProperty(keyValues: Iterable[(String, Any)], className: String)(handleProperty: PartialFunction[(String, Any), Unit]): Unit =
    for (kv ← keyValues) handleProperty.applyOrElse(kv, { o: (String, Any) ⇒ throwNoSuchProperty(o, className = className) })

  /** Like C++ error Z-REMOTE-126 */
  private def throwNoSuchProperty(kv: (String, Any), className: String) =
    throw new NoSuchElementException(s"Unknown local proxy property ${kv._1}='${kv._2}' for $className")

  def requireNoProperties(keyValues: Iterable[(String, Any)], className: String): Unit =
    require(keyValues.isEmpty, s"Unknown local proxy properties: ${keyValues map { _._1 } mkString ","} for $className")
}
