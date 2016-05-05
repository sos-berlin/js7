package com.sos.scheduler.engine.minicom.idispatch

/**
  * @author Joacim Zschimmer
  */
trait InvocableIDispatchMixin extends IDispatch with InvocableIDispatch {
  this: Invocable ⇒

  def getIdOfName(name: String): DISPID

  def invoke(dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any] = Nil, namedArguments: Seq[(DISPID, Any)] = Nil): Any

  def invocable = this
}
