package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.folder.FolderPath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class JobChainQueryTest extends FreeSpec {

  "All" in {
    val q = JobChainQuery.All
    assert(q == JobChainQuery("/"))
    assert(q.reduce == JobChainQuery.All)
    assert(q matches JobChainPath("/a"))
    assert(q matches JobChainPath("/a/b"))
    assert(q.folderPath == FolderPath.Root)
  }

  "Single JobChainPath" in {
    val q = JobChainQuery(JobChainPath("/a/b"))
    assert(q == JobChainQuery("/a/b"))
    assert(q.reduce == JobChainPath("/a/b"))
    assert(!(q matches JobChainPath("/a")))
    assert(q matches JobChainPath("/a/b"))
    assert(!(q matches JobChainPath("/a/b/c")))
    assert(q.folderPath == FolderPath("/a"))
  }

  "Single FolderPath" in {
    val q = JobChainQuery(FolderPath("/a"))
    assert(q == JobChainQuery("/a/"))
    intercept[IllegalArgumentException] { JobChainQuery(FolderPath("/a/")) }
    assert(q.reduce == FolderPath("/a"))
    assert(!(q matches JobChainPath("/a")))
    assert(!(q matches JobChainPath("/x")))
    assert(!(q matches JobChainPath("/x/a")))
    assert(q matches JobChainPath("/a/b"))
    assert(q matches JobChainPath("/a/b/c"))
    assert(q.folderPath == FolderPath("/a"))
  }
}
