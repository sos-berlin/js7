package js7.common.scalautil

import js7.common.scalautil.AssignableFrom.assignableFrom
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.*
import scala.reflect.ClassTag


/**
 * @author Joacim Zschimmer
 */
final class AssignableFromTest extends AnyFreeSpec {

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
