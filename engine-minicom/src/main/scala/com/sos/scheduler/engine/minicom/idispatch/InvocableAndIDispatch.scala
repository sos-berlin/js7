package com.sos.scheduler.engine.minicom.idispatch

/**
  * @author Joacim Zschimmer
  */
trait InvocableAndIDispatch extends Invocable with IDispatch {
  private val invocableIDispatch = XInvocableIDispatch(this)

  def getIdOfName(name: String) = invocableIDispatch.getIdOfName(name)

  def invoke(dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any], namedArguments: Seq[(DISPID, Any)]) =
    invocableIDispatch.invoke(dispId, dispatchTypes, arguments, namedArguments)
}
