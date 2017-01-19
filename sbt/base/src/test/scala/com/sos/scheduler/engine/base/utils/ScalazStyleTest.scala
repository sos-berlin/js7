package com.sos.scheduler.engine.base.utils

import com.sos.scheduler.engine.base.utils.ScalazStyle._
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class ScalazStyleTest extends FreeSpec {

  "Boolean.option" in {
    assert((true option 7: Option[Int]) == Some(7))
    assert((false option 7: Option[Int]) == None)
  }

  "Boolean.list" in {
    assert((true list 7: List[Int]) == List(7))
    assert((false list 7: List[Int]) == Nil)
  }

  "Boolean.vector" in {
    assert((true vector 7: Vector[Int]) == Vector(7))
    assert((false vector 7: Vector[Int]) == Vector())
  }

  "Boolean.set" in {
    assert((true set 7: Set[Int]) == Set(7))
    assert((false set 7: Set[Int]) == Set())
  }
}
