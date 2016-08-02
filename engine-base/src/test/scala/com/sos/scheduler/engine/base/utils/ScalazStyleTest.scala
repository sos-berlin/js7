package com.sos.scheduler.engine.base.utils

import com.sos.scheduler.engine.base.utils.ScalazStyle._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
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
}
