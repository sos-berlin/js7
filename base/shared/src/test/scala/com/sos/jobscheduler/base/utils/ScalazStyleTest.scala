package js7.base.utils

import js7.base.utils.ScalazStyle._
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class ScalazStyleTest extends AnyFreeSpec {

  "Boolean.option" in {
    assert((true option 7: Option[Int]) == Some(7))
    assert((false option 7: Option[Int]) == None)
  }

  "Boolean.thenList" in {
    assert((true thenList 7: List[Int]) == List(7))
    assert((false thenList 7: List[Int]) == Nil)
  }

  "Boolean.thenVector" in {
    assert((true thenVector 7: Vector[Int]) == Vector(7))
    assert((false thenVector 7: Vector[Int]) == Vector())
  }

  "Boolean.thenSet" in {
    assert((true thenSet 7: Set[Int]) == Set(7))
    assert((false thenSet 7: Set[Int]) == Set())
  }
}
