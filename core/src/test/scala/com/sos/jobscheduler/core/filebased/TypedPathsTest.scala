package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.time.Stopwatch.measureTime
import com.sos.jobscheduler.core.filebased.TypedPaths._
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.workflow.WorkflowPath
import java.nio.file.Paths
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TypedPathsTest extends FreeSpec {

  "fileToTypedPath" in {
    assert(fileToTypedPath(Set(WorkflowPath), Paths.get("folder/test.workflow.json")) ==
      Valid(WorkflowPath("/folder/test") → SourceType.Json))
    assert(fileToTypedPath(Set(WorkflowPath, JobPath), Paths.get("folder/test.workflow.json")) ==
      Valid(WorkflowPath("/folder/test") → SourceType.Json))
    assert(fileToTypedPath(Set(WorkflowPath, JobPath), Paths.get("folder/test.job.json")) ==
      Invalid(Problem("File 'folder/test.job.json' is not recognized as a configuration file")))
    assert(fileToTypedPath(Set(WorkflowPath), Paths.get("folder/test.workflow.txt")) ==
      Valid(WorkflowPath("/folder/test") → SourceType.Txt))
    assert(fileToTypedPath(Set(WorkflowPath), Paths.get("folder/test.job_chain.xml")) ==
      Valid(WorkflowPath("/folder/test") → SourceType.Xml))
    assert(fileToTypedPath(Set(WorkflowPath), Paths.get("folder/test.workflow.wrong")) ==
      Invalid(Problem("File 'folder/test.workflow.wrong' is not recognized as a configuration file")))
    assert(fileToTypedPath(Set(WorkflowPath), Paths.get("folder/test.workflow.json")) ==
      Valid(WorkflowPath("/folder/test") → SourceType.Json))
    assert(fileToTypedPath(Set(WorkflowPath), Paths.get("a@b.workflow.json")) ==
      Invalid(Problem("Problem with 'Workflow:/a@b': Invalid character or character combination in name 'a@b'")))
  }

  if (sys.props contains "test.speed") "speed" in {
    val path = Paths.get("folder/test.workflow.json")
    for (_ ← 1 to 5) info(
      measureTime(100000, "fileToTypedPath") {
      fileToTypedPath(Set(WorkflowPath), path)
    }.toString)
  }
}
