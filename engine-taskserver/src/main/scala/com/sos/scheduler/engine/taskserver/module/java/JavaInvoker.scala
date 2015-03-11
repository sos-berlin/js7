package com.sos.scheduler.engine.taskserver.module.java

import com.sos.scheduler.engine.minicom.idispatch.{DISPATCH_METHOD, DISPATCH_PROPERTYGET, DISPATCH_PROPERTYPUT, DispatchType, IDispatch, Invocable, InvocableIDispatch}
import com.sos.scheduler.engine.taskserver.module.java.JavaInvoker._

/**
 * @author Joacim Zschimmer
 */
private[java] class JavaInvoker(iDispatch: IDispatch) extends sos.spooler.Invoker {

  private[java] def call(richName: String, params: Array[AnyRef]) = {
    val (dispatchType, name) = resolveNameSyntax(richName)
    val dispid = iDispatch.getIdOfName(name)
    iDispatch.invoke(dispid, Set(dispatchType), params).asInstanceOf[AnyRef]
  }
}

private[java] object JavaInvoker {
  private[java] def apply(o: Invocable) = new JavaInvoker(InvocableIDispatch(o))

  private[java] def resolveNameSyntax(richName: String): (DispatchType, String) =
    richName.head match {
      case '<' ⇒ (DISPATCH_PROPERTYGET, richName.tail)
      case '>' ⇒ (DISPATCH_PROPERTYPUT, richName.tail)
      case _ ⇒ (DISPATCH_METHOD, richName)
    }
}
