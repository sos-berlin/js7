package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.minicom.idispatch.IDispatch.implicits.RichIDispatch
import com.sos.scheduler.engine.minicom.idispatch.XInvocableIDispatch
import com.sos.scheduler.engine.minicom.idispatch.InvocableIDispatch.implicits._
import com.sos.scheduler.engine.minicom.idispatch.InvocableIDispatchTest._
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.types.HRESULT._
import com.sos.scheduler.engine.minicom.types.{COMException, VariantArray}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.annotation.meta.{getter, setter}

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class InvocableIDispatchTest extends FreeSpec {

  private val a = new A
  private val iDispatch = XInvocableIDispatch(a)

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
    iDispatch.call("optional", List(Int box 1, Double box 2.3)) shouldEqual 3.3
    iDispatch.call("optional", List(1)) shouldEqual 1.0
    iDispatch.call("optional", List()) shouldEqual 0.0
  }

  "GetIDsOfNames and Invoke" in {
    val dispId = iDispatch.getIdOfName("DOUBLE")
    iDispatch.invoke(dispId, Set(DISPATCH_METHOD), List(1.2)) shouldEqual 1.3
  }

  "@invocable is mandatory" in {
    intercept[COMException] { iDispatch.call("noCom") } .hResult shouldEqual DISP_E_UNKNOWNNAME
  }

  "Invocable.call" in {
    a.call("int", List(7)) shouldEqual 8
  }

  "PublicMethodsAreInvocable" in {
    XInvocableIDispatch(B).call("someMethod") shouldEqual 1
  }

  "Underlying IDispatch methods are called, when not overriden by @invocable" in {
    XInvocableIDispatch(C).call("overridden") shouldEqual "OVERRIDDEN"
    XInvocableIDispatch(C).call("notOverridden") shouldEqual "NOT OVERRIDDEN"
  }
  
  "Property" in {
    assert(iDispatch.invokeGet("property") == 0)
    iDispatch.invokePut("property", 7)
    assert(a.property == 7)
    assert(iDispatch.invokeGet("property") == 7)
  }
}

private object InvocableIDispatchTest {
  private val ALong = 111222333444555666L

  private class A extends AnnotatedInvocable {
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
    @invocable def optional(a: Option[java.lang.Integer], b: Option[java.lang.Double]) = {
      // As long as InvocableIDispatch uses Java reflection is used, only Option[_ <: AnyRef] can be used as parameter type.
      val aa = a map { o => o: Int }
      val bb = b map { o => o: Double }
      (aa getOrElse 0) + (bb getOrElse 0.0)
    }
    @(invocable @getter @setter) var property = 0
    def noCom(): Unit = {}
  }

  private object B extends PublicMethodsAreInvocable {
    def someMethod = 1
  }

  private object C extends IDispatch with AnnotatedInvocable {
    @invocable def overridden = "OVERRIDDEN"

    override def getIdOfName(name: String) = name match {
      case "overridden" ⇒ fail("overridden used")
      case "notOverridden" ⇒ DISPID(2)
    }

    override def invoke(dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any], namedArguments: Seq[(DISPID, Any)]) = {
      assert(dispatchTypes == Set(DISPATCH_METHOD))
      (dispId, arguments, namedArguments) match {
        case (DISPID(2), Nil, Nil) ⇒ "NOT OVERRIDDEN"
      }
    }
  }
}
