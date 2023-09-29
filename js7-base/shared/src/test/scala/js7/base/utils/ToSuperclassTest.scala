package js7.base.utils

import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import scala.collection.immutable.HashSet

final class ToSuperclassTest extends OurTestSuite:
  "checked" in:
    val superclasses = Set[Class[? <: Iterable[?]]](
      classOf[Seq[?]],
      classOf[Set[?]])
    val toSuperClass = new ToSuperclass(superclasses)

    assert(toSuperClass.checked(classOf[Seq[?]]) == Right(classOf[Seq[?]]))
    assert(toSuperClass.checked(classOf[List[?]]) == Right(classOf[Seq[?]]))

    assert(toSuperClass.checked(classOf[Set[?]]) == Right(classOf[Set[?]]))
    assert(toSuperClass.checked(classOf[HashSet[?]]) == Right(classOf[Set[?]]))

    assert(toSuperClass.checked(classOf[Map[?, ?]]) == Left(Problem(
      "Unknown scala.collection.Iterable class: scala.collection.immutable.Map")))

    assert(toSuperClass.inspect == Map(
      classOf[Seq[?]]     -> Some(classOf[Seq[?]]),
      classOf[List[?]]    -> Some(classOf[Seq[?]]),
      classOf[Set[?]]     -> Some(classOf[Set[?]]),
      classOf[HashSet[?]] -> Some(classOf[Set[?]]),
      classOf[Map[?, ?]]  -> None))
