package com.sos.scheduler.engine.taskserver.module.java

import com.sos.scheduler.engine.minicom.idispatch.{DISPATCH_METHOD, DISPATCH_PROPERTYGET, DISPATCH_PROPERTYPUT, DISPID, IDispatch}
import com.sos.scheduler.engine.taskserver.module.java.JavaInvokerTest._
import org.junit.runner.RunWith
import org.mockito.Mockito._
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar.mock

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class JavaInvokerTest extends FreeSpec {

  private val iDispatch = mock[IDispatch]
  when(iDispatch.getIdOfName("name")) thenReturn TestDispatchId
  when(iDispatch.invoke(TestDispatchId, Set(DISPATCH_METHOD), List(1, "zwei"))).thenReturn(MethodResult, Nil: _*)
  when(iDispatch.invoke(TestDispatchId, Set(DISPATCH_PROPERTYGET), Nil)).thenReturn(GetPropertyResult, Nil: _*)
  when(iDispatch.invoke(TestDispatchId, Set(DISPATCH_PROPERTYPUT), List("value"))).thenReturn(Unit, Nil: _*)
  private val javaInvoker = new JavaInvoker(iDispatch)

  "call method" in {
    assert(javaInvoker.call("name", Array(Integer valueOf 1, "zwei")) == MethodResult)
  }

  "call <get-property" in {
    assert(javaInvoker.call("<name", Array()) == GetPropertyResult)
  }

  "call >put-property" in {
    assert(javaInvoker.call(">name", Array("value")) == Unit)
  }
}

object JavaInvokerTest {
  private object MethodResult
  private object GetPropertyResult
  private val TestDispatchId = DISPID(42)
}
