package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyIDispatch
import com.sos.scheduler.engine.minicom.types.{CLSID, IUnknown}

/**
  * @author Joacim Zschimmer
  */
trait ProxyRegistering {
  private[remoting] def iUnknown(proxyId: ProxyId): IUnknown

  private[remoting] def newProxy(proxyId: ProxyId, name: String, proxyClsid: CLSID, properties: Iterable[(String, Any)]): ProxyIDispatch
}

object ProxyRegistering {

  private[serial] object NoProxyingRegistering extends ProxyRegistering {
    private[remoting] def iUnknown(proxyId: ProxyId) =
      throw new UnsupportedOperationException("NoProxyRegistering")

    private[remoting] def newProxy(proxyId: ProxyId, name: String, proxyClsid: CLSID, properties: Iterable[(String, Any)]) =
      throw new UnsupportedOperationException("NoProxyRegistering")
  }
}
