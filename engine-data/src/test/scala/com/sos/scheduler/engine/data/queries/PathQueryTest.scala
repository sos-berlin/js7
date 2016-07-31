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
    assert(q == PathQuery("/"))
    assert(q.reduce[JobPath] == PathQuery.All)
    assert(q matches JobPath("/a"))
    assert(q matches JobPath("/a/b"))
    assert(q.folderPath == FolderPath.Root)
  }

  "Single JobPath" in {
    val q = PathQuery(JobPath("/a/b"))
    assert(q == PathQuery("/a/b"))
    assert(q.reduce[JobPath] == JobPath("/a/b"))
    assert(q.reduce[JobChainPath] == JobChainPath("/a/b"))
    assert(!(q matches JobPath("/a")))
    assert(q matches JobPath("/a/b"))
    assert(!(q matches JobPath("/a/b/c")))
    assert(q.folderPath == FolderPath("/a"))
  }

  "PathQuery is not type-safe" in {
    val q = PathQuery(JobPath("/a"))
    assert(q matches JobPath("/a"))
    assert(q matches JobChainPath("/a"))
    assert(q matches FolderPath("/a"))
    assert(q.reduce[JobPath] == JobPath("/a"))
    assert(q.reduce[JobChainPath] == JobChainPath("/a"))
    assert(q.reduce[FolderPath] == FolderPath("/a"))
  }

  "Single FolderPath" in {
    val q = PathQuery(FolderPath("/a"))
    assert(q == PathQuery("/a/"))
    intercept[IllegalArgumentException] { PathQuery(FolderPath("/a/")) }
    assert(q.reduce[JobPath] == FolderPath("/a"))
    assert(!(q matches JobPath("/a")))
    assert(!(q matches JobPath("/x")))
    assert(!(q matches JobPath("/x/a")))
    assert(q matches JobPath("/a/b"))
    assert(q matches JobPath("/a/b/c"))
    assert(q.folderPath == FolderPath("/a"))
  }
}
