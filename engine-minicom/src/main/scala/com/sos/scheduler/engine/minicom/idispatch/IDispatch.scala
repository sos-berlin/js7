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

      def invokeGet(dispId: DISPID, arguments: Seq[Any]): Any =
        delegate.invoke(dispId, Set(DISPATCH_PROPERTYGET), arguments)

      def invokePut(name: String, value: Any): Unit = invokePut(delegate.getIdOfName(name), value)

      def invokePut(dispId: DISPID, value: Any): Unit =
        delegate.invoke(dispId, Set(DISPATCH_PROPERTYPUT), namedArguments = List(DISPID.PROPERTYPUT → value))

      def invokePut(dispId: DISPID, arguments: Seq[Any], value: Any): Any =
        delegate.invoke(dispId, Set(DISPATCH_PROPERTYPUT), arguments = arguments, namedArguments = List(DISPID.PROPERTYPUT → value))

      def invokeMethod(name: String, arguments: Seq[Any] = Nil): Any = invokeMethod(delegate.getIdOfName(name), arguments)

      def invokeMethod(dispId: DISPID, arguments: Seq[Any]): Any =
        delegate.invoke(dispId, Set(DISPATCH_METHOD), arguments)
    }
  }
}
