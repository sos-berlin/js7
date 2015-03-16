package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.minicom.idispatch.InvocableIDispatch._
import com.sos.scheduler.engine.minicom.types.HRESULT._
import com.sos.scheduler.engine.minicom.types.{COMException, VariantArray}
import java.lang.reflect.{InvocationTargetException, Method}

/**
 * Converts an `Invocable` to an `IDispatch`, exhibiting the methods of an possible `IDispatch`.
 * First the methods of the `Invocable` are checked.
 * If the `Invocable` is an `IDispatch` itself, its methods are used, too.
 *
 * @author Joacim Zschimmer
 */
final case class InvocableIDispatch(invocable: Invocable) extends IDispatch {

  private val methods = invocable.invocableMethods.toVector

  def call(name: String, arguments: Seq[Any] = Nil): Any =
    methodIndexOption(name) map { i ⇒ invokeMethod(methods(i), arguments) } getOrElse {
      invocable match {
        case o: IDispatch ⇒ o.invoke(o.getIdOfName(name), Set(DISPATCH_METHOD), arguments)
        case _ ⇒ throw new COMException(DISP_E_UNKNOWNNAME, s"Unknown name '$name'")
      }
    }

  def getIdOfName(name: String) =
    methodIndexOption(name) map methodIndexToDISPID getOrElse {
      invocable match {
        case o: IDispatch ⇒
          val dispid = o.getIdOfName(name)
          if (isInvocableDISPID(dispid)) throw new RuntimeException(s"IDispatch $dispid of ${invocable.getClass} $name collides with DISPIDs of Invocable")  // !!!
          dispid
        case _ ⇒ throw new COMException(DISP_E_UNKNOWNNAME, s"Unknown name '$name'")
      }
    }

  private def methodIndexOption(name: String): Option[Int] = {
    val methodIndices = methods.indices filter { i ⇒ methods(i).getName.compareToIgnoreCase(name) == 0 }
    if (methodIndices.size > 1) throw new COMException(DISP_E_UNKNOWNNAME, s"Name '$name' is ambiguous in ${invocable.getClass}")
    methodIndices.headOption
  }

  def invoke(dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any] = Nil, namedArguments: Seq[(DISPID, Any)] = Nil): Any = {
    if (isInvocableDISPID(dispId)) {
      if (dispatchTypes != Set(DISPATCH_METHOD)) throw new COMException(DISP_E_MEMBERNOTFOUND, "Only DISPATCH_METHOD is supported")
      val method = methods(DISPIDToMethodIndex(dispId))
      if (namedArguments.nonEmpty) throw new COMException(DISP_E_PARAMNOTFOUND, "Named arguments are not supported")
      invokeMethod(method, arguments)
    } else
      invocable match {
        case iDispatch: IDispatch ⇒ iDispatch.invoke(dispId, dispatchTypes, arguments, namedArguments)
        case _ ⇒ throw new COMException(DISP_E_MEMBERNOTFOUND, s"$dispId in ${invocable.getClass}")
      }
  }

  private def invokeMethod(method: Method, arguments: Seq[Any]): Any = {
    if (arguments.size != method.getParameterCount) throw new COMException(DISP_E_BADPARAMCOUNT, s"Number of arguments (${arguments.size}) does not match method $method")
    val javaParameters = for ((t, v) ← method.getParameterTypes zip arguments) yield convert(t.asInstanceOf[Class[_ <: AnyRef]], v)
    val result =
      try method.invoke(invocable, javaParameters: _*)
      catch { case e: InvocationTargetException ⇒ throw e.getTargetException }
    if (result == null) Unit else result
  }

  private def isInvocableDISPID(o: DISPID) = o.value <= MethodDispatchIdBase && o.value > MethodDispatchIdBase - methods.size
}

object InvocableIDispatch {
  object implicits {
    implicit class RichInvocable(val delegate: Invocable) extends AnyVal {
      def call(methodName: String, arguments: Seq[Any]): Any = new InvocableIDispatch(delegate).call(methodName, arguments)
    }
  }

  private val MethodDispatchIdBase = -1000000000
  private def methodIndexToDISPID(i: Int) = DISPID(MethodDispatchIdBase - i)
  private def DISPIDToMethodIndex(o: DISPID) = -(o.value - MethodDispatchIdBase)

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
