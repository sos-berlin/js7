package js7.base.generic

import cats.instances.list._
import cats.syntax.foldable._
import cats.syntax.monoid._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CompletedTest extends AnyFreeSpec
{
  "combine" in {
    assert((Completed |+| Completed) == Completed)
    assert(((Completed: Completed) |+| (Completed: Completed)) == Completed)
  }

  "combineAll" in {
    assert(List(Completed, Completed, Completed).combineAll == Completed)
    assert(List[Completed]().combineAll == Completed)
  }
}
