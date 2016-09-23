package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.filebased.AbsolutePath._
import com.sos.scheduler.engine.data.filebased.AbsolutePathTest._
import com.sos.scheduler.engine.data.folder.FolderPath
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

  "makeAbsolute" in {
    assert(TestPath.makeAbsolute("a") == TestPath("/a"))
    assert(TestPath.makeAbsolute("a/b") == TestPath("/a/b"))
    intercept[IllegalArgumentException] { TestPath.makeAbsolute("./b") }
  }

  "makeAbsolute with default folder" in {
    assert(TestPath.makeAbsolute(FolderPath("/default"), "a") == TestPath("/default/a"))
    assert(TestPath.makeAbsolute(FolderPath("/default"), "/a") == TestPath("/a"))
    assert(TestPath.makeAbsolute(FolderPath("/default"), "./a") == TestPath("/default/a"))
    assert(TestPath.makeAbsolute(FolderPath.Root, "./a") == TestPath("/a"))
    assert(TestPath.makeAbsolute(FolderPath("/default/x"), "/a/b") == TestPath("/a/b"))
    assert(TestPath.makeAbsolute(FolderPath("/default/x"), "a/b") == TestPath("/default/x/a/b"))
    assert(TestPath.makeAbsolute(FolderPath("/default/x"), "./a/b") == TestPath("/default/x/a/b"))
    intercept[Exception] { TestPath.makeAbsolute(FolderPath(""), ")./a") }
    intercept[Exception] { TestPath.makeAbsolute(FolderPath("x"), "./a") }
  }

  "name" in {
    assert(TestPath("/name").name == "name")
    assert(TestPath("/a/b/name").name == "name")
    assert(FolderPath("/").name == "")
  }

  "folder" in {
    assert(TestPath("/a").parent == FolderPath.Root)
    assert(TestPath("/folder/a").parent == FolderPath("/folder"))
    assert(TestPath("/x/folder/a").parent == FolderPath("/x/folder"))
    intercept[IllegalStateException] { FolderPath("/").parent }
  }

  "nesting" in {
    assert(FolderPath("/").nesting == 0)
    assert(TestPath("/a").nesting == 1)
    assert(TestPath("/a/b").nesting == 2)
  }

  "withoutStartingSlash" in {
    assert(TestPath("/a").withoutStartingSlash == "a")
    assert(FolderPath("/").withoutStartingSlash == "")
  }

  "withTrailingSlash" in {
    assert(TestPath("/a").withTrailingSlash == "/a/")
    assert(FolderPath("/").withTrailingSlash == "/")
  }
}

object AbsolutePathTest {
  private case class TestPath(string: String) extends AbsolutePath {
    validate()
    def companion = TestPath
  }

  private object TestPath extends AbsolutePath.Companion[TestPath]
}
