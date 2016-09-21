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

  private type AnyPath = JobPath  // PathQuery may apply to any TypedPath. Here, we select JobPath.
  private val AnyPath = JobPath

  "All" in {
    val q = PathQuery.All
    assert(q.patternString == "/")
    assert(q.toUriPath == "/")
    assert(q == PathQuery.All)
    assert(q matches AnyPath("/a"))
    assert(q matches AnyPath("/a/b"))
    assert(q.folderPath == FolderPath.Root)
  }

  "Single JobPath" in {
    val q = PathQuery(AnyPath("/a/b"))
    assert(q.patternString == "/a/b")
    assert(q.toUriPath == "/a/b")
    assert(q == PathQuery.SinglePath("/a/b"))
    assert(!(q matches AnyPath("/a")))
    assert(q matches AnyPath("/a/b"))
    assert(!(q matches AnyPath("/a/b/c")))
    assert(q.folderPath == FolderPath("/a"))
  }

  "PathQuery may apply to any TypedPath" in {
    val q = PathQuery(AnyPath("/a"))
    assert(q.patternString == "/a")
    assert(q.toUriPath == "/a")
    assert(q matches JobPath("/a"))
    assert(q matches JobChainPath("/a"))
    assert(q matches FolderPath("/a"))
    assert(q == PathQuery.SinglePath("/a"))
  }

  "FolderPath" in {
    val q: PathQuery.Folder = PathQuery(FolderPath("/a"), isRecursive = true)
    assert(q.patternString == "/a/")
    assert(q.toUriPath == "/a/")
    assert(q == PathQuery(FolderPath("/a")))
    assert(q == PathQuery[AnyPath]("/a/"))
    assert(q == PathQuery.FolderTree(FolderPath("/a")))
    intercept[IllegalArgumentException] { PathQuery(FolderPath("/a/")) }
    checkFolderQuery(q)
    assert(q == PathQuery.FolderTree(FolderPath("/a")))
    assert(q matches AnyPath("/a/b/c"))
  }

  "FolderPath, not recursive" in {
    val q = PathQuery(FolderPath("/a"), isRecursive = false)
    assert(q.patternString == "/a/*")
    assert(q.toUriPath == "/a/*")
    assert(q == PathQuery.FolderOnly(FolderPath("/a")))
    checkFolderQuery(q)
    assert(!(q matches AnyPath("/a/b/c")))
  }

  "Root folder, not recursive" in {
    // Special handling for pattern "/*", because FolderPath.Root is "/", not "".
    val q = PathQuery[AnyPath]("/*")
    assert(q.patternString == "/*")
    assert(q.toUriPath == "/*")
    assert(q == PathQuery.FolderOnly(FolderPath.Root))
  }

  private def checkFolderQuery(q: PathQuery.Folder) {
    assert(!(q matches AnyPath("/a")))
    assert(!(q matches AnyPath("/x")))
    assert(!(q matches AnyPath("/x/a")))
    assert(q matches AnyPath("/a/b"))
    assert(q.folderPath == FolderPath("/a"))
  }
}
