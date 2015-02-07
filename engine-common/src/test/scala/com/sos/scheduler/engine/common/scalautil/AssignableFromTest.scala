package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.scalautil.AssignableFrom.assignableFrom
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.reflect.ClassTag


/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class AssignableFromTest extends FreeSpec {

  "AssignableFrom" in {
    trait I
    case class X(x: Int) extends I
    val list = List("x", X(2), "y")
    intercept[ClassCastException] { assignableFrom[I].apply(list.head) }
    def single[A: ClassTag](o: Any): A = assignableFrom[A].apply(o)
    (single[String](list.head): String) shouldEqual "x"
    list collect assignableFrom[String] shouldEqual List("x", "y")
    def select[A: ClassTag](o: List[Any]): List[A] = o collect assignableFrom[A]
    select[String](list) shouldEqual List("x", "y")
    select[I](list) shouldEqual List(X(2))
  }
}
