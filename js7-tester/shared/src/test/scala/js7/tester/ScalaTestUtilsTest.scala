package js7.tester

import js7.tester.ScalaTestUtils.awaitAndAssert
import org.scalatest.exceptions.TestFailedException
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.{Duration, MILLISECONDS}

final class ScalaTestUtilsTest extends AnyFreeSpec
:
  "awaitAndAssert" in:
    pending // TODO Scala 3: try as a macro !
    val a = 1
    val t = try
      awaitAndAssert(Duration(10, MILLISECONDS))(a == 3)
      sys.error("Unreachable code")
    catch { case t: TestFailedException => t }
    assert(t.getMessage == "1 did not equal 3")
