package com.sos.scheduler.engine.minicom.remoting.proxy

import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType}
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId

/**
 * @author Joacim Zschimmer
 */
trait ClientRemoting {

  private[remoting] def getIdOfName(proxyId: ProxyId, name: String): DISPID

  def invoke(
    proxyId: ProxyId,
    dispId: DISPID,
    dispatchTypes: Set[DispatchType],
    arguments: Seq[Any],
    namedArguments: Seq[(DISPID, Any)]): Any
}
