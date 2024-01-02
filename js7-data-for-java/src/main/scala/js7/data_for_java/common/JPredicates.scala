package js7.data_for_java.common

object JPredicates:
  def toScalaPredicate[A](predicate: java.util.function.Predicate[A]): A => Boolean =
    predicate.test
