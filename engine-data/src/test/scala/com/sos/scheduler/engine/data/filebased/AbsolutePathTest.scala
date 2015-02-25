package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.filebased.AbsolutePath._
import com.sos.scheduler.engine.data.filebased.AbsolutePathTest._
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class AbsolutePathTest extends FreeSpec {

  "makeAbsolute" in {
    assert(makeAbsolute("a") == "/a")
    assert(makeAbsolute("a/b") == "/a/b")
    intercept[IllegalArgumentException] { makeAbsolute("./b") }
  }

  "makeCompatibleAbsolute" in {
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
    assert(TestPath("/a").parent == "/")
    assert(TestPath("/a/").parent == "/a")
    assert(TestPath("/folder/a").parent == "/folder")
    assert(TestPath("/x/folder/a").parent == "/x/folder")
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
}
