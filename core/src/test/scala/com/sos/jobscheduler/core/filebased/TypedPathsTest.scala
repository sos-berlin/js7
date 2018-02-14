package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.time.Stopwatch.measureTime
import com.sos.jobscheduler.core.filebased.TypedPaths._
import com.sos.jobscheduler.data.workflow.{JobPath, WorkflowPath}
import java.nio.file.Paths
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TypedPathsTest extends FreeSpec {

  "jsonFileToTypedPath" in {
    val dir = Paths.get("/some/dir")
    assert(jsonFileToTypedPath[WorkflowPath](dir / "folder/test.workflow.json", stripDirectory = dir) == WorkflowPath("/folder/test"))
    assert(jsonFileToTypedPath[WorkflowPath](dir / "folder/test.workflow.json", stripDirectory = Paths.get(s"$dir/")) == WorkflowPath("/folder/test"))
  }

  "xmlFileToTypedPath" in {
    val dir = Paths.get("/some/dir")
    assert(xmlFileToTypedPath[JobPath](dir / "folder/test.job.xml", stripDirectory = dir) == JobPath("/folder/test"))
    assert(xmlFileToTypedPath[JobPath](dir / "folder/test.job.xml", stripDirectory = Paths.get(s"$dir/")) == JobPath("/folder/test"))
  }

  "relativeXmlFilePathToTypedPath" in {
    assert(relativeFilePathToTypedPath[JobPath](_.xmlFilenameExtension, """folder\test.job.xml""") == JobPath("/folder/test"))
    assert(relativeFilePathToTypedPath[JobPath](_.xmlFilenameExtension, "folder/test.job.xml") == JobPath("/folder/test"))
    intercept[IllegalArgumentException] {
      relativeFilePathToTypedPath[JobPath](_.xmlFilenameExtension, "/folder/test.job.xml")
    }
  }

  if (sys.props contains "test.speed") "speed" in {
    val dir = Paths.get("/some/dir")
    val path = dir / "folder/test.job.xml"
    for (_ ← 1 to 10) info(
      measureTime(100000, "xmlFileToTypedPath") {
      xmlFileToTypedPath[JobPath](path, stripDirectory = dir)
    }.toString)
  }
}
