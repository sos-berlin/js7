package com.sos.scheduler.engine.minicom.idispatch

/**
 * @author Joacim Zschimmer
 */
trait IDispatch extends Invocable {

  def getIdOfName(name: String): DISPID

  def invoke(dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any] = Nil, namedArguments: Seq[(DISPID, Any)] = Nil): Any
}

object IDispatch {
  object implicits {
    implicit class RichIDispatch(val delegate: IDispatch) extends AnyVal {

      def invokeGet(name: String): Any = invokeGet(delegate.getIdOfName(name))

      def invokeGet(dispId: DISPID): Any =
        delegate.invoke(dispId, Set(DISPATCH_PROPERTYGET))

      def invokePut(dispId: DISPID, argument: Any): Unit =
        delegate.invoke(dispId, Set(DISPATCH_PROPERTYPUT), namedArguments = List(DISPID.PROPERTYPUT → argument))

      def invokeMethod(dispId: DISPID, arguments: Seq[Any]): Any =
        delegate.invoke(dispId, Set(DISPATCH_METHOD), arguments)
    }
  }
}
