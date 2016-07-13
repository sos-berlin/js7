package com.sos.scheduler.engine.data.folder

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class FolderPathTest extends FreeSpec {

  "lastName" in {
    assert(FolderPath("/").lastName == "")
    assert(FolderPath("/a").lastName == "a")
    assert(FolderPath("/a/").lastName == "a")
    assert(FolderPath("/a/b").lastName == "b")
  }

  "nesting" in {
    assert(FolderPath("/").nesting == 0)
    assert(FolderPath("/a").nesting == 1)
    assert(FolderPath("/a/").nesting == 1)
    assert(FolderPath("/a/b").nesting == 2)
  }

  "slash" in {
    assert(FolderPath("/") / "x" == FolderPath("/x"))
    assert(FolderPath("/a") / "x" == FolderPath("/a/x"))
    assert(FolderPath("/a/") / "x" == FolderPath("/a/x"))
    assert(FolderPath("/a/b") / "x" == FolderPath("/a/b/x"))
    intercept[IllegalArgumentException] { FolderPath("/") / "/" }
    intercept[IllegalArgumentException] { FolderPath("/") / "/x" }
    intercept[IllegalArgumentException] { FolderPath("/") / "x/" }
    intercept[IllegalArgumentException] { FolderPath("/") / "x/y" }
  }
}
