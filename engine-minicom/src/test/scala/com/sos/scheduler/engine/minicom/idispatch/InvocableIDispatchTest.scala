package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.minicom.idispatch.IDispatch.implicits.RichIDispatch
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

  private val annotated = new Annotated

  "call" in {
    annotated.call("int", List(7)) shouldEqual 8
    annotated.call("int", List(Int box 7)) shouldEqual 8
    annotated.call("boxedInteger", List(7)) shouldEqual 8
    annotated.call("boxedInteger", List(Int box 7)) shouldEqual 8
    annotated.call("long", List(ALong)) shouldEqual ALong + 1
    annotated.call("long", List(Long box ALong)) shouldEqual ALong + 1
    annotated.call("long", List(7)) shouldEqual 8
    annotated.call("long", List(Int box 7)) shouldEqual 8
    annotated.call("boxedLong", List(7L)) shouldEqual 8
    annotated.call("boxedLong", List(Long box 7)) shouldEqual 8
    annotated.call("boxedLong", List(7)) shouldEqual 8
    annotated.call("boxedLong", List(Int box 7)) shouldEqual 8
    annotated.call("double", List(1.2)) shouldEqual 1.3
    annotated.call("double", List(Double box 1.2)) shouldEqual 1.3
    annotated.call("boxedDouble", List(1.2)) shouldEqual 1.3
    annotated.call("boxedDouble", List(Double box 1.2)) shouldEqual 1.3
    annotated.call("boolean", List(false)) shouldEqual true
    annotated.call("boolean", List(Boolean box false)) shouldEqual true
    annotated.call("boxedBoolean", List(false)) shouldEqual true
    annotated.call("boxedBoolean", List(Boolean box false)) shouldEqual true
    annotated.call("string", List(1, ALong, 1.2, true, "x")) shouldEqual s"1 $ALong 1.2 true x"
    annotated.call("array", List(VariantArray(Vector[Any](1L, 1.2)))) shouldEqual 2.2
    annotated.call("optional", List(Int box 1, Double box 2.3)) shouldEqual 3.3
    annotated.call("optional", List(1)) shouldEqual 1.0
    annotated.call("optional", List()) shouldEqual 0.0
    annotated.call("var42", List()) shouldEqual 4242
  }

  "GetIDsOfNames and Invoke" in {
    val dispId = annotated.getIdOfName("DOUBLE")
    annotated.invoke(dispId, Set(DISPATCH_METHOD), List(1.2)) shouldEqual 1.3
  }

  "@invocable with dispId" in {
    assert(annotated.invoke(DISPID(42), Set(DISPATCH_METHOD)) == 4242)
    assert(annotated.invoke(DISPID(42), Set(DISPATCH_PROPERTYGET)) == 4242)
  }

  "@invocable is mandatory" in {
    intercept[COMException] { annotated.call("noCom") } .hResult shouldEqual DISP_E_UNKNOWNNAME
  }

  "Invocable.call" in {
    annotated.call("int", List(7)) shouldEqual 8
  }

  "PublicMethodsAreInvocable" in {
    Public.call("someMethod") shouldEqual 1
  }

  "Underlying IDispatch methods are called, when not overriden by @invocable" in {
    OverridingAnnotated.call("overridden") shouldEqual "OVERRIDDEN"
    OverridingAnnotated.call("notOverridden") shouldEqual "NOT OVERRIDDEN"
  }

  "Property" in {
    assert(annotated.invokeGet("property") == 0)
    annotated.invokePut("property", 7)
    assert(annotated.property == 7)
    assert(annotated.invokeGet("property") == 7)
  }
}

private object InvocableIDispatchTest {
  private val ALong = 111222333444555666L

  private class Annotated extends AnnotatedInvocable with InvocableIDispatch {
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
    @(invocable @getter @setter)(dispId = 42) var var42 = 4242
    @invocable def optional(a: Option[java.lang.Integer], b: Option[java.lang.Double]) = {
      // As long as OverridingInvocableIDispatch uses Java reflection is used, only Option[_ <: AnyRef] can be used as parameter type.
      val aa = a map { o => o: Int }
      val bb = b map { o => o: Double }
      (aa getOrElse 0) + (bb getOrElse 0.0)
    }
    @(invocable @getter @setter) var property = 0
    def noCom(): Unit = {}
  }

  private object Public extends PublicMethodsAreInvocable with InvocableIDispatch {
    def someMethod = 1
  }

  private trait MyIDispatch extends IDispatch {
    def getIdOfName(name: String) = name match {
      case "overridden" ⇒ fail("overridden used")
      case "notOverridden" ⇒ DISPID(2)
    }

    def invoke(dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any], namedArguments: Seq[(DISPID, Any)]) = {
      assert(dispatchTypes == Set(DISPATCH_METHOD))
      (dispId, arguments, namedArguments) match {
        case (DISPID(2), Nil, Nil) ⇒ "NOT OVERRIDDEN"
      }
    }
  }

  private object OverridingAnnotated extends MyIDispatch with AnnotatedInvocable with OverridingInvocableIDispatch {
    @invocable def overridden = "OVERRIDDEN"
  }
}
