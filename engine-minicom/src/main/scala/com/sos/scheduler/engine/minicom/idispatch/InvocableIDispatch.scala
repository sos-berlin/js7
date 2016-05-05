package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversableOnce
import com.sos.scheduler.engine.minicom.idispatch.IDispatch.implicits.RichIDispatch
import com.sos.scheduler.engine.minicom.idispatch.InvocableIDispatch._
import com.sos.scheduler.engine.minicom.idispatch.annotation.invocable
import com.sos.scheduler.engine.minicom.types.HRESULT._
import com.sos.scheduler.engine.minicom.types.{COMException, VariantArray}
import java.lang.reflect.{InvocationTargetException, Method, ParameterizedType, Type}
import scala.collection.immutable

/**
 * Converts an with @`invocable` annotated `Invocable` to an `IDispatch`.
 * If the `Invocable` is itself an `IDispatch`, its methods are used when not implemented by @`invocable`.
 * Thus with an `IDispatch`, the methods can be overridden by @`invocable` to implement a proxy.
 * <p>
 * LIMITATION: The `DISPID`s of the invocable `IDispatch` must not be in the interval
 * (`MethodDispatchIdBase` - n, `MethodDispatchIdBase`], where n is the number of `@invocable` annotated names.
 *
 * @author Joacim Zschimmer
 */
trait InvocableIDispatch extends IDispatch {
  protected def invocable: Invocable

  private val (nameToDispid, methodMap) = {
    val seq: Seq[MethodMeta] =
      (invocable.invocableMethods map { o ⇒ o.getName.toLowerCase → o } collect {
        case (ScalaSetterExtractor(name), m) ⇒ List(MethodMeta(DISPATCH_PROPERTYPUT, name, m))
        case (name, m) if m.getReturnType.getName == "void" ⇒ MethodMeta(DISPATCH_METHOD, name, m) :: Nil
        case (name, m) ⇒ MethodMeta(DISPATCH_PROPERTYGET, name, m) :: MethodMeta(DISPATCH_METHOD, name, m) :: Nil
      }).flatten
    val names = (seq map { _.name }).toSet
    val nameToDispid: Map[String, DISPID] = (names zip (Iterator.from(0) map methodIndexToDISPID).toIterable).toMap
    val methodMap: Map[(DispatchType, DISPID), immutable.Iterable[Method]] =
      seq groupBy { o ⇒ (o.typ, nameToDispid(o.name)) } mapValues { v ⇒ (v map { _.method }).toImmutableIterable }
    (nameToDispid, methodMap)
  }

  final def call(name: String, arguments: Seq[Any] = Nil): Any =
    nameToDispid.get(name.toLowerCase) match {
      case Some(dispid) ⇒ invokeMethod(method(DISPATCH_METHOD, dispid), arguments)
      case None ⇒
        invocable match {
          case o: IDispatch ⇒ o.invokeMethod(o.getIdOfName(name), arguments)
          case _ ⇒ throw new COMException(DISP_E_UNKNOWNNAME, s"Unknown name '$name'in $getClass")
        }
    }

  final def getIdOfName(name: String) =
    nameToDispid.getOrElse(name.toLowerCase,
      invocable match {
        case o: IDispatch ⇒
          val dispid = o.getIdOfName(name)
          if (isInvocableDISPID(dispid)) throw new RuntimeException(s"IDispatch $dispid of ${invocable.getClass} $name collides with DISPIDs of Invocable") // !!!
          dispid
        case _ ⇒ throw new COMException(DISP_E_UNKNOWNNAME, s"Unknown name '$name' in $getClass")
      }
    )

  final def invoke(dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any] = Nil, namedArguments: Seq[(DISPID, Any)] = Nil): Any = {
    if (isInvocableDISPID(dispId)) {
      if (dispatchTypes.size != 1) throw new COMException(DISP_E_MEMBERNOTFOUND, "Use of multiple or no DispatchType is not supported")
      val dispatchType = dispatchTypes.head
      val m = method(dispatchType, dispId)
      dispatchType match {
        case DISPATCH_PROPERTYGET | DISPATCH_METHOD ⇒
          if (namedArguments.nonEmpty) throw new COMException(DISP_E_PARAMNOTFOUND, "Named arguments are not supported")
          invokeMethod(m, arguments)
        case DISPATCH_PROPERTYPUT ⇒
          if (namedArguments.size != 1 || namedArguments.head._1 != DISPID.PROPERTYPUT) throw new COMException(DISP_E_PARAMNOTFOUND, "Unexpected named argument for DISPATCH_PROPERTYPUT")
          invokeMethod(m, arguments :+ namedArguments.head._2)
        case _ ⇒ throw new COMException(DISP_E_MEMBERNOTFOUND, "Only DISPATCH_METHOD, DISPATCH_PROPERTYGET and DISPATCH_PROPERTYPUT is supported")
      }
    } else
      invocable match {
        case iDispatch: IDispatch ⇒ iDispatch.invoke(dispId, dispatchTypes, arguments, namedArguments)
        case _ ⇒ throw new COMException(DISP_E_MEMBERNOTFOUND, s"Unknown $dispId in ${invocable.getClass}")
      }
  }

  private def invokeMethod(method: Method, arguments: Seq[Any]): Any = {
    val optionalCount = method.getParameterTypes.reverse prefixLength { _ == classOf[Option[_]] }
    val n = method.getParameterCount
    if (arguments.size < n - optionalCount || arguments.size > n) throw new COMException(DISP_E_BADPARAMCOUNT, s"Number of arguments (${arguments.size}) does not match method $method")
    val javaParameters = for ((t, v) ← method.getGenericParameterTypes.zipAll(arguments, classOf[Nothing], MissingArgument)) yield convert(t, v)
    val result =
      try method.invoke(invocable, javaParameters: _*)
      catch { case e: InvocationTargetException ⇒ throw e.getTargetException }
    if (result == null) Unit else result
  }

  private def isInvocableDISPID(o: DISPID) = o.value <= MethodDispatchIdBase && o.value > MethodDispatchIdBase - nameToDispid.size

  private def method(t: DispatchType, dispId: DISPID): Method = {
    val m = methodMap((t, dispId))
    if (m.size > 1) throw new COMException(DISP_E_MEMBERNOTFOUND, s"Multiple methods for $dispId '${dispIdToName(dispId)}' in ${invocable.getClass}")
    m.headOption getOrElse { throw new COMException(DISP_E_MEMBERNOTFOUND, s"Unknown @invocable $dispId in ${invocable.getClass}") }
  }

  private def dispIdToName(o: DISPID): String = (nameToDispid find { _._2 == o }).get._1
}

object InvocableIDispatch {
  object implicits {
    implicit class RichInvocable(val delegate: Invocable) extends AnyVal {
      def call(methodName: String, arguments: Seq[Any]): Any = XInvocableIDispatch(delegate).call(methodName, arguments)
    }
  }

  private val MethodDispatchIdBase = -1000000000
  private def methodIndexToDISPID(i: Int) = DISPID(MethodDispatchIdBase - i)

  private val ScalaSetterExtractor = """(.+)_\$eq""".r   // Scala-generated method name for setter "property_=" ends with "_$eq"

  private val IntClass = classOf[Int]
  private val BoxedIntegerClass = classOf[java.lang.Integer]
  private val LongClass = classOf[Long]
  private val BoxedLongClass = classOf[java.lang.Long]
  private val DoubleClass = classOf[Double]
  private val BoxedDoubleClass = classOf[java.lang.Double]
  private val BooleanClass = classOf[Boolean]
  private val BoxedBooleanClass = classOf[java.lang.Boolean]
  private val StringClass = classOf[String]
  private val AnyRefClass = classOf[AnyRef]
  private val VariantArraySerializableClass = classOf[VariantArray]

  private def convert(typ: Type, value: Any): AnyRef =
    typ match {
      case IntClass | BoxedIntegerClass ⇒ value match {
        case o: Int ⇒ Int box o
      }
      case LongClass | BoxedLongClass ⇒ value match {
        case o: Int ⇒ Long box o
        case o: Long ⇒ Long box o
      }
      case DoubleClass | BoxedDoubleClass ⇒ value match {
        case o: Double ⇒ Double box o
      }
      case BooleanClass | BoxedBooleanClass ⇒ value match {
        case o: Boolean ⇒ Boolean box o
      }
      case StringClass ⇒ value.toString
      case AnyRefClass ⇒ value.asInstanceOf[AnyRef]
      case VariantArraySerializableClass ⇒ value.asInstanceOf[VariantArray]
      case typ: ParameterizedType if typ.getRawType == classOf[Option[_]] ⇒
        val Array(t) = typ.getActualTypeArguments   // A Scala simple type (like Int) is returned as classOf[AnyRef] by Java reflection. Use Option[_ <: AnyRef] instead !!!
        value match {
          case MissingArgument ⇒ None
          case _ ⇒ Some(convert(t, value))
        }
      case _ ⇒ throw new IllegalArgumentException(s"Unsuported type for dynamic method invocation: $typ")
    }

  private case class MethodMeta(typ: DispatchType, name: String, method: Method)

  private object MissingArgument
}
