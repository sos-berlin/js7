package js7.base.generic

import cats.instances.list.*
import cats.kernel.Monoid
import cats.syntax.foldable.*
import cats.syntax.monoid.*
import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class CompletedTest extends OurTestSuite
{
  "empty" in {
    assert(Monoid[Completed].empty eq Completed)
  }

  "combine" in {
    assert((Completed |+| Completed) eq Completed)
    assert(((Completed: Completed) |+| (Completed: Completed)) eq Completed)
  }

  "combineAll" in {
    assert(List(Completed, Completed, Completed).combineAll eq Completed)
    assert(List.empty[Completed].combineAll eq Completed)
  }

  "fill" in {
    assert(Monoid[Completed].combineN(Completed, 7) eq Completed)
  }

  "reverse.combineAll" in {
    assert(Monoid[Completed].reverse.combineAll(List(Completed, Completed)) eq Completed)
  }

  //"fold" in {
  //  assert(List(Completed, Completed).fold == Completed)
  //}

  "unorderedFold" in {
    assert(List(Completed, Completed).unorderedFold == Completed)
  }
}
