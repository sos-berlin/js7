package com.sos.scheduler.engine.shared.filebased

import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.time.Stopwatch.measureTime
import com.sos.scheduler.engine.data.engine2.order.JobPath
import com.sos.scheduler.engine.shared.filebased.TypedPaths.{fileToTypedPath, relativeFilePathToTypedPath}
import java.nio.file.Paths
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TypedPathsTest extends FreeSpec {

  "fileToTypedPath" in {
    val dir = Paths.get("/some/dir")
    assert(fileToTypedPath[JobPath](dir / "folder/test.job.xml", stripDirectory = dir) == JobPath("/folder/test"))
    assert(fileToTypedPath[JobPath](dir / "folder/test.job.xml", stripDirectory = Paths.get(s"$dir/")) == JobPath("/folder/test"))
  }

  "relativeFilePathToTypedPath" in {
    assert(relativeFilePathToTypedPath[JobPath]("""folder\test.job.xml""") == JobPath("/folder/test"))
    assert(relativeFilePathToTypedPath[JobPath]("folder/test.job.xml") == JobPath("/folder/test"))
    intercept[IllegalArgumentException] {
      relativeFilePathToTypedPath[JobPath]("/folder/test.job.xml")
    }
  }

  if (sys.props contains "test.speed")
  "speed" in {
    val dir = Paths.get("/some/dir")
    val path = dir / "folder/test.job.xml"
    for (_ ← 1 to 10) measureTime(100000, "fileToTypedPath") {
      fileToTypedPath[JobPath](path, stripDirectory = dir)
    }
  }
}
