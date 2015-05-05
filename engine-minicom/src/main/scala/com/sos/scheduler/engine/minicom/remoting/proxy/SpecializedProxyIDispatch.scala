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
  def forEachProperty(keyValues: Iterable[(String, Any)])(handleProperty: PartialFunction[(String, Any), Unit]): Unit = {
    for (kv ← keyValues) handleProperty.applyOrElse(kv, { o: (String, Any) ⇒ throwNoSuchProperty(o._1) })
  }

  private def throwNoSuchProperty(name: String) = throw new NoSuchElementException(s"Unknown local proxy property '$name' for sos.spooler.Log")
}
