package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType, IDispatch, Invocable}
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.{ClientRemoting, ProxyIDispatch}
import com.sos.scheduler.engine.minicom.types.CLSID

/**
 * @author Joacim Zschimmer
 */
trait ServerRemoting {
  private[remoting] def newProxy(proxyId: ProxyId, name: String, proxyClsid: CLSID, properties: Iterable[(String, Any)]): IDispatch

  private[remoting] def invocable(proxyId: ProxyId): Invocable
}

object ServerRemoting {
  object Dummy extends ServerRemoting {
    def newProxy(proxyId: ProxyId, name: String, proxyClsid: CLSID, properties: Iterable[(String, Any)]): IDispatch =
      new DummyProxy(proxyId, name)

    def invocable(proxyId: ProxyId): Invocable = Invocable.Empty
  }

  private final class DummyProxy(val id: ProxyId, val name: String) extends ProxyIDispatch {
    protected val remoting = EmptyClientRemoting
  }

  private object EmptyClientRemoting extends ClientRemoting {
    private[remoting] def getIdOfName(proxyId: ProxyId, name: String) =
      throw new UnsupportedOperationException

    def invoke(proxyId: ProxyId, dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any], namedArguments: Seq[(DISPID, Any)]) =
      throw new UnsupportedOperationException
  }
}
