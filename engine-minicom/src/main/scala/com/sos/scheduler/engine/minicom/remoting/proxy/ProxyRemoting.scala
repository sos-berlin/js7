package com.sos.scheduler.engine.minicom.remoting.proxy

import com.sos.scheduler.engine.base.generic.Completed
import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType}
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
trait ProxyRemoting {

  def release(proxyId: ProxyId): Future[Completed]

  /**
    * Combines getIdOfName and invoke.
    * May be used to optimize remote procedure calls.
    */
  def call(proxyId: ProxyId, methodName: String, arguments: Seq[Any]): Future[Any]

  def getIdOfName(proxyId: ProxyId, name: String): Future[DISPID]

  def invoke(
    proxyId: ProxyId,
    dispId: DISPID,
    dispatchTypes: Set[DispatchType],
    arguments: Seq[Any],
    namedArguments: Seq[(DISPID, Any)]): Future[Any]
}
