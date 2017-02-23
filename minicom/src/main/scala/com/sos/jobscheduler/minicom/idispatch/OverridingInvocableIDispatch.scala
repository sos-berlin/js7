package com.sos.scheduler.engine.minicom.idispatch

import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.minicom.idispatch.OverridingInvocableIDispatch._
import com.sos.scheduler.engine.minicom.types.HRESULT._
import com.sos.scheduler.engine.minicom.types.{COMException, VariantArray}
import java.lang.reflect.{InvocationTargetException, Method, ParameterizedType, Type}

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
trait OverridingInvocableIDispatch extends Invocable with IDispatch {

  private lazy val (nameToDispId, dispIdToDispatchTypeToMethod) = {
    val invocableMethods = this.invocableMethods
    val nameToDispId: Map[String, DISPID] = {
      val dispIdGenerator = for (i ← Iterator from 0) yield DISPID(MethodDispatchIdBase - i)
      val normalized = invocableMethods map { case (method, dispIdOption) ⇒ normalizeName(method.getName) → dispIdOption }
      (for ((name, dispIdOption) ← normalized.distinct) yield
        name → (dispIdOption getOrElse dispIdGenerator.next()))
      .distinct.uniqueToMap
    }
    val methodMetas: Seq[MethodMeta] = {
      (invocableMethods map {
        case (m, Some(dispId)) ⇒ m → dispId
        case (m, None) ⇒ m → nameToDispId(normalizeName(m.getName))
      } map { m ⇒
        m._1.getName → m
      } collect {
        case (ScalaSetterExtractor(name), (m, dispId)) ⇒
          MethodMeta(DISPATCH_PROPERTYPUT, name, m, dispId) :: Nil
        case (name, (m, dispId)) if m.getReturnType.getName == "void" ⇒
          MethodMeta(DISPATCH_METHOD, normalizeName(name), m, dispId) :: Nil
        case (name, (m, dispId)) ⇒
          MethodMeta(DISPATCH_PROPERTYGET, normalizeName(name), m, dispId) ::
          MethodMeta(DISPATCH_METHOD, normalizeName(name), m, dispId) :: Nil
      }).flatten
    }
    val dispIdToDispatchTypeToMethod: Map[DISPID, Map[DispatchType, Method]] =
      methodMetas groupBy { _.dispId } mapValues { _ groupBy { _.typ }} map { case (dispId, dispatchTypeToMethodMetas) ⇒
        dispId → (dispatchTypeToMethodMetas mapValues { metas ⇒
          if (metas.size == 1)
            metas.head.method
          else
            throw new COMException(DISP_E_MEMBERNOTFOUND, s"Multiple methods ${metas map { _.method } mkString ", "} for $dispId in $getClass")
        })
      }
    (nameToDispId, dispIdToDispatchTypeToMethod)
  }

  abstract override def call(name: String, arguments: Seq[Any] = Nil): Any =
    nameToDispId.get(normalizeName(name)) match {
      case Some(dispId) ⇒ invokeMethod(dispIdToDispatchTypeToMethod(dispId)(DISPATCH_METHOD), arguments)
      case None ⇒ super.call(name, arguments)
    }

  abstract override def getIdOfName(name: String) = {
    def superDispid = {
      val dispid = super.getIdOfName(name)
      if (dispIdToDispatchTypeToMethod contains dispid) sys.error(s"IDispatch $dispid of $getClass $name collides with DISPIDs of Invocable")
      dispid
    }
    nameToDispId.getOrElse(normalizeName(name), superDispid)
  }

  abstract override def invoke(dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any] = Nil, namedArguments: Seq[(DISPID, Any)] = Nil): Any =
    dispIdToDispatchTypeToMethod.get(dispId) match {
      case Some(dispatchTypeToMethod) ⇒
        if (dispatchTypes.size != 1) throw new COMException(DISP_E_MEMBERNOTFOUND, "Use of multiple or no DispatchType is not supported")
        val dispatchType = dispatchTypes.head
        val method = dispatchTypeToMethod.getOrElse(dispatchType, throw new NoSuchElementException(s"$getClass: $dispId is not accessible via $dispatchType"))
        dispatchType match {
          case DISPATCH_PROPERTYGET | DISPATCH_METHOD ⇒
            if (namedArguments.nonEmpty) throw new COMException(DISP_E_PARAMNOTFOUND, "Named arguments are not supported")
            invokeMethod(method, arguments)
          case DISPATCH_PROPERTYPUT ⇒
            if (namedArguments.size != 1 || namedArguments.head._1 != DISPID.PROPERTYPUT)
              throw new COMException(DISP_E_PARAMNOTFOUND, "Unexpected named argument for DISPATCH_PROPERTYPUT")
            invokeMethod(method, arguments :+ namedArguments.head._2)
          case _ ⇒ throw new COMException(DISP_E_MEMBERNOTFOUND, "Only DISPATCH_METHOD, DISPATCH_PROPERTYGET and DISPATCH_PROPERTYPUT is supported")
        }
      case None ⇒
        super.invoke(dispId, dispatchTypes, arguments, namedArguments)
    }

  private def invokeMethod(method: Method, arguments: Seq[Any]): Any = {
    val optionalCount = method.getParameterTypes.reverse prefixLength { _ == classOf[Option[_]] }
    val n = method.getParameterCount
    if (arguments.size < n - optionalCount || arguments.size > n)
      throw new COMException(DISP_E_BADPARAMCOUNT, s"Number of arguments (${arguments.size}) does not match method $method")
    val javaParameters = for ((t, v) ← method.getGenericParameterTypes.zipAll(arguments, classOf[Nothing], MissingArgument)) yield convert(t, v)
    val result =
      try method.invoke(this, javaParameters: _*)
      catch { case e: InvocationTargetException ⇒ throw e.getTargetException }
    if (result == null) Unit else result
  }
}

object OverridingInvocableIDispatch {
  private val MethodDispatchIdBase = -1000000000
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

  private def normalizeName(name: String): String =
    name match {
      case ScalaSetterExtractor(name_) ⇒ name_.toLowerCase
      case _ ⇒ name.toLowerCase
    }

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

  private case class MethodMeta(typ: DispatchType, name: String, method: Method, dispId: DISPID)

  private object MissingArgument
}
