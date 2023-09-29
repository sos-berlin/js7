package js7.common.scalautil

import js7.base.utils.ScalaUtils.implicitClass
import scala.reflect.ClassTag

/**
 * @author Joacim Zschimmer
 */
private class AssignableFrom[A : ClassTag] extends PartialFunction[Any, A] {
  def apply(o: Any) = {
    if !isDefinedAt(o) then throw new ClassCastException(s"Not a ${implicitClass[A].getName}: '$o' (${o.getClass})")
    o.asInstanceOf[A]
  }

  def isDefinedAt(o: Any) = implicitClass[A] isAssignableFrom o.getClass
}

object AssignableFrom {
  /**
   * @return A [[PartialFunction]]`[Any, A]`, defined iff the argument is an `A`, and then returning the argument as an `A`
   */
  def assignableFrom[A : ClassTag]: PartialFunction[Any, A] = new AssignableFrom[A]
}
