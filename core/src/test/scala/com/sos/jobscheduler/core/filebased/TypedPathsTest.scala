package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.time.Stopwatch.measureTime
import com.sos.jobscheduler.core.filebased.TypedPaths._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.workflow.WorkflowPath
import java.io.File.separator
import java.nio.file.Paths
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TypedPathsTest extends FreeSpec {

  "fileToTypedPath" in {
    val dir = Paths.get("DIR")
    assert(fileToTypedPath(Set(WorkflowPath), dir, dir / "folder/test.workflow.json") ==
      Valid(WorkflowPath("/folder/test") → SourceType.Json))
    assert(fileToTypedPath(Set(WorkflowPath, AgentPath), dir, dir / "folder/test.workflow.json") ==
      Valid(WorkflowPath("/folder/test") → SourceType.Json))
    assert(fileToTypedPath(Set(WorkflowPath, AgentPath), dir, dir / "folder/test.workflow.yaml") ==
      Valid(WorkflowPath("/folder/test") → SourceType.Yaml))
    assert(fileToTypedPath(Set(WorkflowPath), dir, dir / "folder/test.workflow.txt") ==
      Valid(WorkflowPath("/folder/test") → SourceType.Txt))
    assert(fileToTypedPath(Set(WorkflowPath), dir, dir / "folder/test.job_chain.xml") ==
      Valid(WorkflowPath("/folder/test") → SourceType.Xml))
    assert(fileToTypedPath(Set(WorkflowPath), dir, dir / "folder/test.workflow.wrong") ==
      Invalid(Problem(s"File '...${separator}folder${separator}test.workflow.wrong' is not recognized as a configuration file")))
    assert(fileToTypedPath(Set(WorkflowPath), dir, dir / "folder/test.workflow.json") ==
      Valid(WorkflowPath("/folder/test") → SourceType.Json))
    assert(fileToTypedPath(Set(WorkflowPath), dir, dir / "a@b.workflow.json") ==
      Invalid(Problem("Problem with 'Workflow:/a@b': Invalid character or character combination in name 'a@b'")))
  }

  if (sys.props contains "test.speed") "speed" in {
    val dir = Paths.get("/TEST/JOBSCHEDULER/PROVIDER/CONFIG/LIVE")
    val path = dir / "folder/test.workflow.json"
    for (_ ← 1 to 5) info(
      measureTime(100000, "fileToTypedPath") {
      fileToTypedPath(Set(WorkflowPath), dir, path)
    }.toString)
  }
}
