package com.sos.scheduler.engine.minicom.remoting.proxy

import com.sos.scheduler.engine.base.generic.Completed
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.time.ScalaTime.MaxDuration
import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType, IDispatch}
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
trait ProxyIDispatch extends IDispatch {
  val id: ProxyId
  val name: String
  protected val remoting: ProxyRemoting

  def release(): Future[Completed] =
    remoting.release(id)

  override def call(methodName: String, arguments: Seq[Any]): Any =
    asyncCall(methodName, arguments) await MaxDuration

  def asyncCall(methodName: String, arguments: Seq[Any]): Future[Any] =
    remoting.call(id, methodName, arguments)

  def getIdOfName(name: String): DISPID =
    remoting.getIdOfName(id, name)
      .await(MaxDuration)

  def invoke(dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any] = Nil, namedArguments: Seq[(DISPID, Any)] = Nil): Any =
    remoting.invoke(id, dispId, dispatchTypes, arguments, namedArguments)
      .await(MaxDuration)

  override def toString = s"ProxyIDispatch($name)"
}
