package com.sos.scheduler.engine.data.folder

import com.sos.scheduler.engine.data.job.JobPath
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

  "isAncestorOf" in {
    assert(!FolderPath("/a").isAncestorOf(JobPath("/x")))
    assert(!FolderPath("/a/b").isAncestorOf(JobPath("/a")))
    assert(!FolderPath("/a/b").isAncestorOf(JobPath("/a/b")))
    assert(FolderPath("/") isAncestorOf JobPath("/x"))
    assert(FolderPath("/a/b") isAncestorOf JobPath("/a/b/c"))
    assert(FolderPath("/a/b") isAncestorOf JobPath("/a/b/c/d"))
  }

  "fromTrailingSlash" in {
    intercept[IllegalArgumentException] { FolderPath.fromTrailingSlash("") }
    intercept[IllegalArgumentException] { FolderPath.fromTrailingSlash("/a") }
    assert(FolderPath.fromTrailingSlash("/") == FolderPath.Root)
    assert(FolderPath.fromTrailingSlash("/a/") == FolderPath("/a"))
  }
}
