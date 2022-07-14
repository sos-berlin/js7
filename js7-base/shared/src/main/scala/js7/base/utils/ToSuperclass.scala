package js7.base.utils

import java.util.concurrent.ConcurrentHashMap
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Classes.superclassesOf
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

/** Map for subclasses to any of the given superclasses. */
final class ToSuperclass[C](superclasses: Set[Class[? <: C]])(implicit C: ClassTag[C])
{
  private val map = new ConcurrentHashMap[Class[? <: C], Option[Class[? <: C]]]
  map.putAll(superclasses.view.map(o => o -> Some(o)).toMap.asJava)

  def checked[C1 <: C](cls: Class[C1]): Checked[Class[? <: C]] =
    get(cls).toRight(Problem.lzy(
      s"Unknown ${C.runtimeClass.getName} class: ${cls.getName}"))

  def contains[C1 <: C](cls: Class[C1]): Boolean =
    get(cls).nonEmpty

  def get[C1 <: C](cls: Class[C1]): Option[Class[? <: C]] =
    map.get(cls) match {
      case null =>
        synchronized {
          map.get(cls) match {
            case null =>
              val maybeSuperclass = superclassesOf(cls)
                .view
                .flatMap(superclass => Option(map.get(superclass.asInstanceOf[Class[? <: C]])))
                .headOption
                .flatten
              map.put(cls, maybeSuperclass)
              maybeSuperclass
            case maybeSuperclass => maybeSuperclass
          }
        }
      case maybeSuperclass => maybeSuperclass
    }

  private[utils] def inspect = map.asScala
}
