package com.sos.scheduler.engine.minicom.remoting.proxy

import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType, IDispatch}
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId

/**
 * @author Joacim Zschimmer
 */
trait ProxyIDispatch extends IDispatch {
  val id: ProxyId
  val name: String
  protected val remoting: ProxyRemoting

  override def call(methodName: String, arguments: Seq[Any]): Any =
    remoting.call(id, methodName, arguments)

  def getIdOfName(name: String): DISPID =
    remoting.getIdOfName(id, name)

  def invoke(dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any] = Nil, namedArguments: Seq[(DISPID, Any)] = Nil): Any =
    remoting.invoke(id, dispId, dispatchTypes, arguments, namedArguments)

  override def toString = s"ProxyIDispatch($name)"
}
