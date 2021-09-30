package js7.base.utils

import js7.base.problem.Problem
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.HashSet

final class ToSuperclassTest extends AnyFreeSpec
{
  "checked" in {
    val superclasses = Set[Class[_ <: Iterable[_]]](
      classOf[Seq[_]],
      classOf[Set[_]])
    val toSuperClass = new ToSuperclass(superclasses)

    assert(toSuperClass.checked(classOf[Seq[_]]) == Right(classOf[Seq[_]]))
    assert(toSuperClass.checked(classOf[List[_]]) == Right(classOf[Seq[_]]))

    assert(toSuperClass.checked(classOf[Set[_]]) == Right(classOf[Set[_]]))
    assert(toSuperClass.checked(classOf[HashSet[_]]) == Right(classOf[Set[_]]))

    assert(toSuperClass.checked(classOf[Map[_, _]]) == Left(Problem(
      "Unknown scala.collection.Iterable class: scala.collection.immutable.Map")))

    assert(toSuperClass.inspect == Map(
      classOf[Seq[_]]     -> Some(classOf[Seq[_]]),
      classOf[List[_]]    -> Some(classOf[Seq[_]]),
      classOf[Set[_]]     -> Some(classOf[Set[_]]),
      classOf[HashSet[_]] -> Some(classOf[Set[_]]),
      classOf[Map[_, _]]  -> None))
  }
}
