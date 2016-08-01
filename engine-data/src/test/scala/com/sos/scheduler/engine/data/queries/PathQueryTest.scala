package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.folder.FolderPath
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class PathQueryTest extends FreeSpec {

  "All" in {
    val q = PathQuery.All
    assert(q.patternString == "/")
    assert(q == PathQuery.All)
    assert(q matches JobPath("/a"))
    assert(q matches JobPath("/a/b"))
    assert(q.folderPath == FolderPath.Root)
  }

  "Single JobPath" in {
    val q = PathQuery(JobPath("/a/b"))
    assert(q.patternString == "/a/b")
    assert(q == PathQuery.SinglePath("/a/b"))
    assert(!(q matches JobPath("/a")))
    assert(q matches JobPath("/a/b"))
    assert(!(q matches JobPath("/a/b/c")))
    assert(q.folderPath == FolderPath("/a"))
  }

  "PathQuery is not type-safe" in {
    val q = PathQuery(JobPath("/a"))
    assert(q.patternString == "/a")
    assert(q matches JobPath("/a"))
    assert(q matches JobChainPath("/a"))
    assert(q matches FolderPath("/a"))
    assert(q == PathQuery.SinglePath("/a"))
  }

  "Single FolderPath" in {
    val q = PathQuery(FolderPath("/a"))
    assert(q == PathQuery("/a/"))
    intercept[IllegalArgumentException] { PathQuery(FolderPath("/a/")) }
    assert(q == PathQuery.Folder(FolderPath("/a")))
    assert(!(q matches JobPath("/a")))
    assert(!(q matches JobPath("/x")))
    assert(!(q matches JobPath("/x/a")))
    assert(q matches JobPath("/a/b"))
    assert(q matches JobPath("/a/b/c"))
    assert(q.folderPath == FolderPath("/a"))
  }
}
