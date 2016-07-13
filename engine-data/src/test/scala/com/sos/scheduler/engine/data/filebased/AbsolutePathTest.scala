package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.filebased.AbsolutePath._
import com.sos.scheduler.engine.data.filebased.AbsolutePathTest._
import com.sos.scheduler.engine.data.folder.FolderPath
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
@SuppressWarnings(Array("deprecated"))
final class AbsolutePathTest extends FreeSpec {

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
    assert(TestPath.makeAbsolute(FolderPath("/default/"), "/a") == TestPath("/a"))
    assert(TestPath.makeAbsolute(FolderPath("/default/"), "a") == TestPath("/default/a"))
    assert(TestPath.makeAbsolute(FolderPath("/default/"), "./a") == TestPath("/default/a"))
    assert(TestPath.makeAbsolute(FolderPath("/default/x"), "/a/b") == TestPath("/a/b"))
    assert(TestPath.makeAbsolute(FolderPath("/default/x"), "a/b") == TestPath("/default/x/a/b"))
    assert(TestPath.makeAbsolute(FolderPath("/default/x"), "./a/b") == TestPath("/default/x/a/b"))
    intercept[Exception] { TestPath.makeAbsolute(FolderPath(""), ")./a") }
    intercept[Exception] { TestPath.makeAbsolute(FolderPath("x"), "./a") }
  }

  "makeCompatibleAbsolute (deprecated)" in {
    assert(makeCompatibleAbsolute("/base", "/a") == "/a")
    assert(makeCompatibleAbsolute("/base", "a") == "/a")
    assert(makeCompatibleAbsolute("/base", "./a") == "/base/a")
    assert(makeCompatibleAbsolute("/", "./a") == "/a")
    assert(makeCompatibleAbsolute("/base/", "/a") == "/a")
    assert(makeCompatibleAbsolute("/base/", "a") == "/a")
    assert(makeCompatibleAbsolute("/base/", "./a") == "/base/a")
    assert(makeCompatibleAbsolute("/base/x", "/a/b") == "/a/b")
    assert(makeCompatibleAbsolute("/base/x", "a/b") == "/a/b")
    assert(makeCompatibleAbsolute("/base/x", "./a/b") == "/base/x/a/b")
    intercept[Exception] { makeCompatibleAbsolute("", "./a") }
    intercept[Exception] { makeCompatibleAbsolute("x", "./a") }
  }

  "name" in {
    assert(TestPath("/name").name == "name")
    assert(TestPath("/a/b/name").name == "name")
    assert(TestPath("/").name == "")
  }

  "folder" in {
    assert(TestPath("/a").parent == FolderPath.Root)
    assert(TestPath("/a/").parent == FolderPath("/a"))
    assert(TestPath("/folder/a").parent == FolderPath("/folder"))
    assert(TestPath("/x/folder/a").parent == FolderPath("/x/folder"))
    intercept[IllegalStateException] { TestPath("/").parent }
  }

  "withoutStartingSlash" in {
    assert(TestPath("/a").withoutStartingSlash == "a")
  }

  "withTrailingSlash" in {
    assert(TestPath("/a").withTrailingSlash == "/a/")
    assert(TestPath("/a/").withTrailingSlash == "/a/")
    assert(TestPath("/").withTrailingSlash == "/")
  }
}

object AbsolutePathTest {
  private case class TestPath(string: String) extends AbsolutePath {
    requireIsAbsolute()
  }

  private object TestPath extends AbsolutePath.Companion[TestPath]
}
