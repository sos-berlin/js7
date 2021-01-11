package js7.base.generic

import cats.instances.list._
import cats.kernel.Monoid
import cats.syntax.foldable._
import cats.syntax.monoid._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CompletedTest extends AnyFreeSpec
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
    assert(List[Completed]().combineAll eq Completed)
  }

  "fill" in {
    assert(Monoid[Completed].combineN(Completed, 7) eq Completed)
  }

  "reverse.combineAll" in {
    assert(Monoid[Completed].reverse.combineAll(List(Completed, Completed)) eq Completed)
  }
}
