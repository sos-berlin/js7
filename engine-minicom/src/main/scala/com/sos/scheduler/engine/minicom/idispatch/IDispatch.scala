package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.minicom.types.HRESULT.DISP_E_UNKNOWNNAME
import com.sos.scheduler.engine.minicom.types.{COMException, IUnknown}

/**
 * @author Joacim Zschimmer
 */
trait IDispatch extends IUnknown {

  def call(name: String, arguments: Seq[Any]): Any = {
    val dispId = getIdOfName(name)
    invoke(dispId, Set(DISPATCH_METHOD), arguments)
  }

  def getIdOfName(name: String): DISPID

  def invoke(dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any] = Nil, namedArguments: Seq[(DISPID, Any)] = Nil): Any
}

object IDispatch {
  object implicits {
    implicit class RichIDispatch(val delegate: IDispatch) extends AnyVal {

      def invokeGet(name: String): Any =
        invokeGet(delegate.getIdOfName(name))

      def invokeGet(dispId: DISPID): Any =
        delegate.invoke(dispId, Set(DISPATCH_PROPERTYGET))

      def invokeGet(dispId: DISPID, arguments: Seq[Any]): Any =
        delegate.invoke(dispId, Set(DISPATCH_PROPERTYGET), arguments)

      def invokePut(name: String, value: Any): Unit =
        invokePut(delegate.getIdOfName(name), value)

      def invokePut(dispId: DISPID, value: Any): Unit =
        delegate.invoke(dispId, Set(DISPATCH_PROPERTYPUT), namedArguments = List(DISPID.PROPERTYPUT → value))

      def invokePut(dispId: DISPID, arguments: Seq[Any], value: Any): Any =
        delegate.invoke(dispId, Set(DISPATCH_PROPERTYPUT), arguments = arguments, namedArguments = List(DISPID.PROPERTYPUT → value))

      def invokeMethod(name: String, arguments: Seq[Any] = Nil): Any =
        invokeMethod(delegate.getIdOfName(name), arguments)

      def invokeMethod(dispId: DISPID, arguments: Seq[Any]): Any =
        delegate.invoke(dispId, Set(DISPATCH_METHOD), arguments)
    }
  }

  trait Empty extends IDispatch {
    def getIdOfName(name: String): DISPID =
      throw new COMException(DISP_E_UNKNOWNNAME, s"Unknown name '$name'in $getClass")

    def invoke(dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any], namedArguments: Seq[(DISPID, Any)]): Any =
      throw new COMException(DISP_E_UNKNOWNNAME, s"Unknown $dispId in $getClass")
  }
}
