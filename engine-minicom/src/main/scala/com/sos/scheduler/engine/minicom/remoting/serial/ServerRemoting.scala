package com.sos.scheduler.engine.minicom.remoting.serial

import akka.util.ByteString
import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType, Invocable}
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.{ClientRemoting, ProxyIDispatch}
import com.sos.scheduler.engine.minicom.types.{CLSID, IUnknown}

/**
 * @author Joacim Zschimmer
 */
trait ServerRemoting {
  private[remoting] def newProxy(proxyId: ProxyId, name: String, proxyClsid: CLSID, properties: Iterable[(String, Any)]): ProxyIDispatch

  private[remoting] def iUnknown(proxyId: ProxyId): IUnknown

  private[remoting] def executeMessage(callMessage: ByteString): ByteString
}

object ServerRemoting {
  private[remoting] object Dummy extends ServerRemoting {
    def newProxy(proxyId: ProxyId, name: String, proxyClsid: CLSID, properties: Iterable[(String, Any)]): ProxyIDispatch =
      new DummyProxy(proxyId, name)

    def iUnknown(proxyId: ProxyId): IUnknown =
      new Invocable.Empty {}

    def executeMessage(callMessage: ByteString) =
      throw new UnsupportedOperationException("ServerRemoting.executeMessage")
  }

  private final class DummyProxy(val id: ProxyId, val name: String) extends ProxyIDispatch {
    protected val remoting = EmptyClientRemoting
  }

  private object EmptyClientRemoting extends ClientRemoting {
    def call(proxyId: ProxyId, methodName: String, arguments: Seq[Any]) =
      throw new UnsupportedOperationException

    private[remoting] def getIdOfName(proxyId: ProxyId, name: String) =
      throw new UnsupportedOperationException

    def invoke(proxyId: ProxyId, dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any], namedArguments: Seq[(DISPID, Any)]) =
      throw new UnsupportedOperationException
  }
}
