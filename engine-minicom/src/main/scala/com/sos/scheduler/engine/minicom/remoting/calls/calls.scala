package com.sos.scheduler.engine.minicom.remoting.calls

import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType}
import com.sos.scheduler.engine.minicom.types.{CLSID, IID, IUnknown}
import javax.annotation.Nullable
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
private[remoting] sealed trait Call


private[remoting] sealed trait SessionCall extends Call


private[remoting] final case class CreateInstanceCall(
  clsid: CLSID,
  @Nullable outer: IUnknown,
  context: Int,
  iids: immutable.Seq[IID])
extends SessionCall


private[remoting] sealed trait ObjectCall extends Call {
  def proxyId: ProxyId
}


private[remoting] final case class ReleaseCall(proxyId: ProxyId)
extends ObjectCall


private[remoting] final case class QueryInterfaceCall(proxyId: ProxyId, iid: IID)
extends ObjectCall


private[remoting] final case class GetIDsOfNamesCall(proxyId: ProxyId, iid: IID, localeId: Int, names: immutable.Seq[String])
extends ObjectCall


private[remoting] final case class InvokeCall(
  proxyId: ProxyId,
  dispatchId: DISPID,
  iid: IID,
  dispatchTypes: immutable.Set[DispatchType],
  arguments: immutable.Seq[Any],
  namedArguments: immutable.Seq[(DISPID, Any)] = Nil)
extends ObjectCall


private[remoting] final case class CallCall(proxyId: ProxyId, methodName: String, arguments: immutable.Seq[Any])
extends ObjectCall

private[remoting] case object KeepAliveCall extends Call
