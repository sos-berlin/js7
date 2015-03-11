package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.minicom.idispatch.InvocableIDispatch.implicits._
import com.sos.scheduler.engine.minicom.idispatch.InvocableIDispatchTest._
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.types.HRESULT._
import com.sos.scheduler.engine.minicom.types.{COMException, VariantArray}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class InvocableIDispatchTest extends FreeSpec {

  private val iDispatch = InvocableIDispatch(A)

  "call" in {
    iDispatch.call("int", List(7)) shouldEqual 8
    iDispatch.call("int", List(Int box 7)) shouldEqual 8
    iDispatch.call("boxedInteger", List(7)) shouldEqual 8
    iDispatch.call("boxedInteger", List(Int box 7)) shouldEqual 8
    iDispatch.call("long", List(ALong)) shouldEqual ALong + 1
    iDispatch.call("long", List(Long box ALong)) shouldEqual ALong + 1
    iDispatch.call("long", List(7)) shouldEqual 8
    iDispatch.call("long", List(Int box 7)) shouldEqual 8
    iDispatch.call("boxedLong", List(7L)) shouldEqual 8
    iDispatch.call("boxedLong", List(Long box 7)) shouldEqual 8
    iDispatch.call("boxedLong", List(7)) shouldEqual 8
    iDispatch.call("boxedLong", List(Int box 7)) shouldEqual 8
    iDispatch.call("double", List(1.2)) shouldEqual 1.3
    iDispatch.call("double", List(Double box 1.2)) shouldEqual 1.3
    iDispatch.call("boxedDouble", List(1.2)) shouldEqual 1.3
    iDispatch.call("boxedDouble", List(Double box 1.2)) shouldEqual 1.3
    iDispatch.call("boolean", List(false)) shouldEqual true
    iDispatch.call("boolean", List(Boolean box false)) shouldEqual true
    iDispatch.call("boxedBoolean", List(false)) shouldEqual true
    iDispatch.call("boxedBoolean", List(Boolean box false)) shouldEqual true
    iDispatch.call("string", List(1, ALong, 1.2, true, "x")) shouldEqual s"1 $ALong 1.2 true x"
    iDispatch.call("array", List(VariantArray(Vector[Any](1L, 1.2)))) shouldEqual 2.2
  }

  "GetIDsOfNames and Invoke" in {
    val dispId = iDispatch.getIdOfName("DOUBLE")
    iDispatch.invoke(dispId, Set(DISPATCH_METHOD), List(1.2)) shouldEqual 1.3
  }

  "@invocable is mandatory" in {
    intercept[COMException] { iDispatch.call("noCom") } .hResult shouldEqual DISP_E_UNKNOWNNAME
  }

  "Invocable.call" in {
    A.call("int", List(7)) shouldEqual 8
  }

  "PublicMethodsAreInvocable" in {
    InvocableIDispatch(B).call("someMethod") shouldEqual 1
  }
}

private object InvocableIDispatchTest {
  private val ALong = 111222333444555666L
  private object A extends Invocable {
    @invocable def int(o: Int) = o + 1
    @invocable def boxedInteger(o: java.lang.Integer): java.lang.Integer = o + 1
    @invocable def long(o: Long) = o + 1
    @invocable def boxedLong(o: java.lang.Long): java.lang.Long = o + 1
    @invocable def double(o: Double) = o + 0.1
    @invocable def boxedDouble(o: java.lang.Double): java.lang.Double = o + 0.1
    @invocable def boolean(o: Boolean) = !o
    @invocable def boxedBoolean(o: Boolean): java.lang.Boolean = !o
    @invocable def string(i: Int, l: Long, d: Double, b: Boolean, o: String) = s"$i $l $d $b $o"
    @invocable def array(a: VariantArray) = a.indexedSeq(0).asInstanceOf[Long] + a.indexedSeq(1).asInstanceOf[Double]
    def noCom(): Unit = {}
  }

  private object B extends PublicMethodsAreInvocable {
    def someMethod = 1
  }
}
