package com.sos.scheduler.engine.data.folder

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class FolderPathTest extends FreeSpec {

  "subfolder" in {
    assert(FolderPath("/").subfolder("x") == FolderPath("/x"))
    assert(FolderPath("/a").subfolder("x") == FolderPath("/a/x"))
    assert(FolderPath("/a/b").subfolder("x") == FolderPath("/a/b/x"))
    intercept[IllegalArgumentException] { FolderPath("/") subfolder "/" }
    intercept[IllegalArgumentException] { FolderPath("/") subfolder "/x" }
    intercept[IllegalArgumentException] { FolderPath("/") subfolder "x/" }
    intercept[IllegalArgumentException] { FolderPath("/") subfolder "x/y" }
  }
}
