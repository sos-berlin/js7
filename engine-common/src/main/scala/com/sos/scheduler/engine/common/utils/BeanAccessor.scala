package com.sos.scheduler.engine.common.utils

import com.sos.scheduler.engine.common.utils.BeanAccessor._
import java.beans.Introspector.getBeanInfo
import java.lang.reflect.Method

/**
  * @author Joacim Zschimmer
  */
final class BeanAccessor[A](classAccessor: BeanClassAccessor[A], obj: A) {

  def apply(key: String): Any =
    classAccessor.getter(key).invoke(obj)

  def update(key: String, value: AnyRef): Unit =
    classAccessor.setter(key).invoke(obj, value)
}

object BeanAccessor {
  def forAny(obj: Any) = new BeanAccessor(new BeanClassAccessor[Any](obj.getClass), obj)

  private case class PropertyAccessor(getter: Option[Method], setter: Option[Method])

  final class BeanClassAccessor[A](clas: Class[_ <: A]) {
    private[BeanAccessor] val properties: Map[String, PropertyAccessor] = (
      for (d ← getBeanInfo(clas).getPropertyDescriptors;
           getter = Option(d.getReadMethod);
           setter = Option(d.getWriteMethod)
           if getter.isDefined && getter.get.getParameterCount == 0 || setter.isDefined && setter.get.getParameterCount == 1)
        yield d.getName → PropertyAccessor(getter, setter)
      ).toMap withDefault { k ⇒ throw new NoSuchElementException(s"No such property '$k'") }

    def getter(key: String): Method =
      properties(key).getter getOrElse {
        throw new NoSuchElementException(s"Property '$key' has not getter")
      }

    def setter(key: String): Method =
      properties(key).setter getOrElse {
        throw new NoSuchElementException(s"Property '$key' has not setter")
      }
  }
}
