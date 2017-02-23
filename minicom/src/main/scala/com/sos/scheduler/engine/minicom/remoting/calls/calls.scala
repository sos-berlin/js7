package com.sos.scheduler.engine.minicom.remoting.calls

import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType}
import com.sos.scheduler.engine.minicom.types.{CLSID, IID, IUnknown}
import javax.annotation.Nullable
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
sealed trait Call


sealed trait SessionCall extends Call


final case class CreateInstanceCall(
  clsid: CLSID,
  @Nullable outer: IUnknown,
  context: Int,
  iids: immutable.Seq[IID])
extends SessionCall


sealed trait ObjectCall extends Call {
  def proxyId: ProxyId
}


final case class ReleaseCall(proxyId: ProxyId)
extends ObjectCall


final case class QueryInterfaceCall(proxyId: ProxyId, iid: IID)
extends ObjectCall


final case class GetIDsOfNamesCall(proxyId: ProxyId, iid: IID, localeId: Int, names: immutable.Seq[String])
extends ObjectCall


final case class InvokeCall(
  proxyId: ProxyId,
  dispatchId: DISPID,
  iid: IID,
  dispatchTypes: immutable.Set[DispatchType],
  arguments: immutable.Seq[Any],
  namedArguments: immutable.Seq[(DISPID, Any)] = Nil)
extends ObjectCall


final case class CallCall(proxyId: ProxyId, methodName: String, arguments: immutable.Seq[Any])
extends ObjectCall

case object KeepaliveCall extends Call
