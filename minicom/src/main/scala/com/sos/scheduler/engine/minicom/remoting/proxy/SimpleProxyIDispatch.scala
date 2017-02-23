package com.sos.scheduler.engine.minicom.remoting.proxy

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.types.CLSID

/**
 * @author Joacim Zschimmer
 */
private[proxy] final class SimpleProxyIDispatch(
  protected val remoting: ProxyRemoting,
  val id: ProxyId,
  val name: String)
extends CachingProxyIDispatch {
  override def toString = s"SimpleProxyIDispatch($name)"
}

private[minicom] object SimpleProxyIDispatch extends ProxyIDispatchFactory {
  val clsid = CLSID.Null
  private val logger = Logger(getClass)

  def apply(remoting: ProxyRemoting, id: ProxyId, name: String, properties: Iterable[(String, Any)]) = {
    if (properties.nonEmpty) logger.warn(s"IGNORED: $properties")
    new SimpleProxyIDispatch(remoting, id, name)
  }
}
