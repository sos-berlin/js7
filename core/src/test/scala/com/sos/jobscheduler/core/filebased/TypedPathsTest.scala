package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.time.Stopwatch.measureTime
import com.sos.jobscheduler.core.filebased.TypedPaths._
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.workflow.{JobPath, WorkflowPath}
import java.nio.file.Paths
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TypedPathsTest extends FreeSpec {

  "fileToTypedPath" in {
    val dir = Paths.get("/some/dir")
    assert(fileToTypedPath(Set(WorkflowPath), dir / "folder/test.workflow.json", stripDirectory = dir) ==
      Valid(WorkflowPath("/folder/test") → SourceType.Json))
    assert(fileToTypedPath(Set(WorkflowPath, JobPath), dir / "folder/test.workflow.json", stripDirectory = dir) ==
      Valid(WorkflowPath("/folder/test") → SourceType.Json))
    assert(fileToTypedPath(Set(WorkflowPath, JobPath), dir / "folder/test.job.json", stripDirectory = dir) ==
      Invalid(Problem("File 'folder/test.job.json' is not recognized as a configuration file")))
    assert(fileToTypedPath(Set(WorkflowPath), dir / "folder/test.workflow.txt", stripDirectory = dir) ==
      Valid(WorkflowPath("/folder/test") → SourceType.Txt))
    assert(fileToTypedPath(Set(WorkflowPath), dir / "folder/test.job_chain.xml", stripDirectory = dir) ==
      Valid(WorkflowPath("/folder/test") → SourceType.Xml))
    assert(fileToTypedPath(Set(WorkflowPath), dir / "folder/test.workflow.wrong", stripDirectory = dir) ==
      Invalid(Problem("File 'folder/test.workflow.wrong' is not recognized as a configuration file")))
    assert(fileToTypedPath(Set(WorkflowPath), dir / "folder/test.workflow.json", stripDirectory = Paths.get(s"$dir/")) ==
      Valid(WorkflowPath("/folder/test") → SourceType.Json))
  }

  if (sys.props contains "test.speed") "speed" in {
    val dir = Paths.get("/some/dir")
    val path = dir / "folder/test.workflow.json"
    for (_ ← 1 to 5) info(
      measureTime(100000, "fileToTypedPath") {
      fileToTypedPath(Set(WorkflowPath), path, stripDirectory = dir)
    }.toString)
  }
}
