package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.filebased.AbsolutePathTest._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@SuppressWarnings(Array("deprecated"))
@RunWith(classOf[JUnitRunner])
final class AbsolutePathTest extends FreeSpec {

  "validate" in {
    intercept[IllegalArgumentException] { TestPath("") }
    intercept[IllegalArgumentException] { TestPath("x") }
    intercept[IllegalArgumentException] { TestPath("/x/") }
    intercept[IllegalArgumentException] { TestPath("x/") }
    intercept[IllegalArgumentException] { TestPath("/x//y") }
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
