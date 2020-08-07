package js7.proxy.javaapi.data

object JPredicates
{
  def toScalaPredicate[A](predicate: java.util.function.Predicate[A]): A => Boolean =
    predicate.test
}
