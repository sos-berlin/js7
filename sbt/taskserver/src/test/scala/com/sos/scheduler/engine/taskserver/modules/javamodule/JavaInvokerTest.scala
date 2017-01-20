package com.sos.scheduler.engine.taskserver.modules.javamodule

import com.sos.scheduler.engine.minicom.idispatch.{DISPATCH_METHOD, DISPATCH_PROPERTYGET, DISPATCH_PROPERTYPUT, DISPID, IDispatch}
import com.sos.scheduler.engine.taskserver.modules.javamodule.JavaInvokerTest._
import org.mockito.Mockito._
import org.scalatest.FreeSpec
import org.scalatest.mock.MockitoSugar.mock

/**
 * @author Joacim Zschimmer
 */
final class JavaInvokerTest extends FreeSpec {

  "call method" in {
    val iDispatch = mockIDispatch()
    when(iDispatch.invoke(TestDispatchId, Set(DISPATCH_METHOD), List(1, "zwei"))).thenReturn(MethodResult, Nil: _*)
    val javaInvoker = new JavaInvoker(iDispatch)
    assert(javaInvoker.call("name", Array(Integer valueOf 1, "zwei")) == MethodResult)
  }

  "call <get-property" in {
    val iDispatch = mockIDispatch()
    when(iDispatch.invoke(TestDispatchId, Set(DISPATCH_PROPERTYGET), Nil)).thenReturn(GetPropertyResult, Nil: _*)
    val javaInvoker = new JavaInvoker(iDispatch)
    assert(javaInvoker.call("<name", Array()) == GetPropertyResult)
  }

  "call >put-property" in {
    val iDispatch = mockIDispatch()
    when(iDispatch.invoke(TestDispatchId, Set(DISPATCH_PROPERTYPUT), Array[Any](), namedArguments = List(DISPID.PROPERTYPUT → "value"))).thenReturn(PutPropertyResult, Nil: _*)
    val javaInvoker = new JavaInvoker(iDispatch)
    assert(javaInvoker.call(">name", Array("value")) == PutPropertyResult)  // Normally, a property-put does not return any value
  }

  "call >put-property with two values" in {
    val iDispatch = mockIDispatch()
    when(iDispatch.invoke(TestDispatchId, Set(DISPATCH_PROPERTYPUT), List("name2"), namedArguments = List(DISPID.PROPERTYPUT → "value"))).thenReturn(Put2PropertyResult, Nil: _*)
    val javaInvoker = new JavaInvoker(iDispatch)
    assert(javaInvoker.call(">name", Array("name2", "value")) == Put2PropertyResult)  // Normally, a property-put does not return any value
  }
}

object JavaInvokerTest {
  private object MethodResult
  private object GetPropertyResult
  private object PutPropertyResult
  private object Put2PropertyResult
  private val TestDispatchId = DISPID(42)

  private def mockIDispatch() = {
    val iDispatch = mock[IDispatch]
    when(iDispatch.getIdOfName("name")) thenReturn TestDispatchId
    iDispatch
  }
}
