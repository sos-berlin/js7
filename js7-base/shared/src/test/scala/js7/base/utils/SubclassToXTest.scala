package js7.base.utils

import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import scala.collection.immutable.HashSet

final class SubclassToXTest extends OurTestSuite:
  "checked" in:
    val superclassToX = Map[Class[? <: Iterable[?]], String](
      classOf[Seq[?]] -> "Seq",
      classOf[Set[?]] -> "Set")
    val toSuperClass = new SubclassToX(superclassToX)

    assert(toSuperClass.checked(classOf[Seq[?]]) == Right("Seq"))
    assert(toSuperClass.checked(classOf[List[?]]) == Right("Seq"))

    assert(toSuperClass.checked(classOf[Set[?]]) == Right("Set"))
    assert(toSuperClass.checked(classOf[HashSet[?]]) == Right("Set"))

    assert(toSuperClass.checked(classOf[Map[?, ?]]) == Left(Problem(
      "Unknown scala.collection.Iterable class: scala.collection.immutable.Map")))
