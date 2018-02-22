package com.sos.jobscheduler.data.filebased

import cats.data.Validated.Invalid
import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import com.sos.jobscheduler.data.filebased.AbsolutePathTest._
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
@SuppressWarnings(Array("deprecated"))
final class AbsolutePathTest extends FreeSpec {

  "validate" in {
    intercept[ProblemException] { TestPath("") }
    intercept[ProblemException] { TestPath("x") }
    intercept[ProblemException] { TestPath("/x/") }
    intercept[ProblemException] { TestPath("x/") }
    intercept[ProblemException] { TestPath("/x//y") }
  }

  "check" in {
    assert(TestPath.checked("x") == Invalid(Problem("Absolute path expected in TestPath 'x'")))
  }

  "name" in {
    assert(TestPath("/name").name == "name")
    assert(TestPath("/a/b/name").name == "name")
  }

  "nesting" in {
    assert(TestPath("/a").nesting == 1)
    assert(TestPath("/a/b").nesting == 2)
  }

  "withoutStartingSlash" in {
    assert(TestPath("/a").withoutStartingSlash == "a")
  }

  "withTrailingSlash" in {
    assert(TestPath("/a").withTrailingSlash == "/a/")
  }
}

object AbsolutePathTest {
  private case class TestPath(string: String) extends AbsolutePath {
    validate()
    def companion = TestPath
  }

  private object TestPath extends AbsolutePath.Companion[TestPath]
}
