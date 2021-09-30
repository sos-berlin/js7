package js7.base.utils

import js7.base.problem.Problem
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.HashSet

final class SubclassToXTest extends AnyFreeSpec
{
  "checked" in {
    val superclassToX = Map[Class[_ <: Iterable[_]], String](
      classOf[Seq[_]] -> "Seq",
      classOf[Set[_]] -> "Set")
    val toSuperClass = new SubclassToX(superclassToX)

    assert(toSuperClass.checked(classOf[Seq[_]]) == Right("Seq"))
    assert(toSuperClass.checked(classOf[List[_]]) == Right("Seq"))

    assert(toSuperClass.checked(classOf[Set[_]]) == Right("Set"))
    assert(toSuperClass.checked(classOf[HashSet[_]]) == Right("Set"))

    assert(toSuperClass.checked(classOf[Map[_, _]]) == Left(Problem(
      "Unknown scala.collection.Iterable class: scala.collection.immutable.Map")))
  }
}
