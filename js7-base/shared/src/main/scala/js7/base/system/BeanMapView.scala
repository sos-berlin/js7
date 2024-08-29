package js7.base.system

import java.beans.PropertyDescriptor
import java.lang.reflect.{InvocationTargetException, Method}
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import scala.collection.MapView
import scala.reflect.ClassTag
import scala.util.control.NonFatal

/**
 * A MapView that dynamically reads bean properties.
 * <p>
 *   Invocation exception is returns as Problem.
 */
final class BeanMapView[A] private(bean: A, nameToMethod: Map[String, Method])
extends MapView[String, Any]:

  def get(name: String): Option[Any] =
    nameToMethod.get(name).map(invoke)

  def iterator: Iterator[(String, Any)] =
    nameToMethod.iterator.map: (name, method) =>
      name -> invoke(method)

  private def invoke(method: Method): Any =
    try
      method.invoke(bean)
    catch case NonFatal(t) =>
      Problem:
        t.match
          case t: InvocationTargetException => t.getCause
          case t => t
        .toStringWithCauses

  override def keysIterator: Iterator[String] =
    nameToMethod.keysIterator

  override def keySet: Set[String] =
    nameToMethod.keySet


object BeanMapView:

  def apply[A: ClassTag](bean: A): BeanMapView[A] =
    apply(bean, AllButClass)

  def apply[A: ClassTag](bean: A, filter: PropertyFilter): BeanMapView[A] =
    Factory[A](filter).toMapView(bean)

  def apply[A](clas: Class[A], bean: A, filter: PropertyFilter = AllButClass): BeanMapView[A] =
    Factory[A](clas, filter).toMapView(bean)


  /// Factory ///

  /** Prepare reflection for fast access to multiple beans of same class. */
  final class Factory[A](clas: Class[A], filter: PropertyFilter = AllButClass):
    private val nameToMethod: Map[String, Method] =
      java.beans.Introspector.getBeanInfo(clas)
        .getPropertyDescriptors
        .filter(filter)
        .flatMap: d =>
          Option(d.getReadMethod)
            .filter: m =>
              m.getParameterCount == 0
            .map: m =>
              d.getName -> m
        .toMap

    def toMapView(bean: A): BeanMapView[A] =
      new BeanMapView[A](bean, nameToMethod)

  object Factory:
    def apply[A: ClassTag]: Factory[A] =
      apply(implicitClass[A])

    def apply[A: ClassTag](filter: PropertyFilter): Factory[A] =
      new Factory(implicitClass[A], filter)

    def apply[A](clas: Class[A], filter: PropertyFilter = AllButClass): Factory[A] =
      new Factory(clas, filter)


  /// PropertyFilter ///

  private type PropertyFilter = PropertyDescriptor => Boolean

  private val AllButClass: PropertyFilter =
    _.getName != "class"

  final class NameFilter(filter: String => Boolean) extends PropertyFilter:
    def apply(d: PropertyDescriptor) = filter(d.getName)
