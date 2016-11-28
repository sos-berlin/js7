package com.sos.scheduler.engine.minicom.remoting.proxy

import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.types.CLSID

/**
 * @author Joacim Zschimmer
 */
trait ProxyIDispatchFactory {
  val clsid: CLSID

  def apply(remoting: ProxyRemoting, id: ProxyId, name: String, proxyProperties: Iterable[(String, Any)]): ProxyIDispatch
}

object ProxyIDispatchFactory {
  type Fun = (ProxyRemoting, ProxyId, String, Iterable[(String, Any)]) ⇒ ProxyIDispatch
}
