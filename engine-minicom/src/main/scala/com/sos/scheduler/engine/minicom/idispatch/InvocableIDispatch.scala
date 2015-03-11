package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.idispatch.InvocableIDispatch._
import com.sos.scheduler.engine.minicom.types.HRESULT._
import com.sos.scheduler.engine.minicom.types.{COMException, VariantArray}
import java.lang.reflect.{InvocationTargetException, Method}

/**
 * @author Joacim Zschimmer
 */
final case class InvocableIDispatch(invocable: Invocable) extends IDispatch {

  private val methods = invocable.invocableMethods

  def call(methodName: String, arguments: Seq[Any] = Nil): Any =
    invokeMethod(methods(methodIndex(methodName)), arguments)

  def getIdOfName(name: String) = DISPID(methodIndex(name))

  private def methodIndex(name: String): Int = {
    val methodIndices = methods.indices filter { i ⇒ methods(i).getName.compareToIgnoreCase(name) == 0 }
    if (methodIndices.size < 1) throw new COMException(DISP_E_UNKNOWNNAME, s"Unknown name '$name'")
    if (methodIndices.size > 1) throw new COMException(DISP_E_UNKNOWNNAME, s"Ambiguous name '$name'")
    methodIndices.head
  }

  def invoke(dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any], namedArguments: Seq[(DISPID, Any)]): Any = {
    if (dispatchTypes != Set(DISPATCH_METHOD)) throw new COMException(DISP_E_MEMBERNOTFOUND, "Only DISPATCH_METHOD is supported")
    val method = methods(dispId.value)
    if (namedArguments.nonEmpty) throw new COMException(DISP_E_PARAMNOTFOUND, "Named arguments are not supported")
    invokeMethod(method, arguments)
  }

  private def invokeMethod(method: Method, arguments: Seq[Any]): Any = {
    if (arguments.size != method.getParameterCount) throw new COMException(DISP_E_BADPARAMCOUNT, s"Number of arguments (${arguments.size}) does not match method $method")
    val javaParameters = for ((t, v) ← method.getParameterTypes zip arguments) yield convert(t.asInstanceOf[Class[_ <: AnyRef]], v)
    val result =
      try method.invoke(invocable, javaParameters: _*)
      catch { case e: InvocationTargetException ⇒ throw e.getTargetException }
    if (result == null) Unit else result
  }
}

object InvocableIDispatch {
  object implicits {
    implicit class RichInvocable(val delegate: Invocable) extends AnyVal {
      def call(methodName: String, arguments: Seq[Any]): Any = new InvocableIDispatch(delegate).call(methodName, arguments)
    }
  }

  private val IntClass = classOf[Int]
  private val BoxedIntegerClass = classOf[java.lang.Integer]
  private val LongClass = classOf[Long]
  private val BoxedLongClass = classOf[java.lang.Long]
  private val DoubleClass = classOf[Double]
  private val BoxedDoubleClass = classOf[java.lang.Double]
  private val BooleanClass = classOf[Boolean]
  private val BoxedBooleanClass = classOf[java.lang.Boolean]
  private val StringClass = classOf[String]
  private val VariantArraySerializableClass = classOf[VariantArray]
  private val logger = Logger(getClass)

  private def convert[A <: AnyRef](c: Class[A], v: Any): A =
    (c match {
      case IntClass | BoxedIntegerClass ⇒ v match {
        case o: Int ⇒ Int box o
      }
      case LongClass | BoxedLongClass ⇒ v match {
        case o: Int ⇒ Long box o
        case o: Long ⇒ Long box o
      }
      case DoubleClass | BoxedDoubleClass ⇒ v match {
        case o: Double ⇒ Double box o
      }
      case BooleanClass | BoxedBooleanClass ⇒ v match {
        case o: Boolean ⇒ Boolean box o
      }
      case StringClass ⇒ v.toString
      case VariantArraySerializableClass ⇒ v.asInstanceOf[VariantArray]
    }).asInstanceOf[A]
}
