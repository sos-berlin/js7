package js7.core.filebased

import js7.base.problem.Problem
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.time.Stopwatch.measureTime
import js7.common.scalautil.FileUtils.syntax._
import js7.core.filebased.TypedPaths._
import js7.data.agent.AgentRefPath
import js7.data.filebased.SourceType
import js7.data.workflow.WorkflowPath
import java.io.File.separator
import java.nio.file.Paths
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TypedPathsTest extends AnyFreeSpec {

  "fileToTypedPathAndSourceType" in {
    val dir = Paths.get("DIR")
    assert(fileToTypedPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.workflow.json") ==
      Right(WorkflowPath("/folder/test") -> SourceType.Json))
    assert(fileToTypedPathAndSourceType(Set(WorkflowPath, AgentRefPath), dir, dir / "folder/test.workflow.json") ==
      Right(WorkflowPath("/folder/test") -> SourceType.Json))
    assert(fileToTypedPathAndSourceType(Set(WorkflowPath, AgentRefPath), dir, dir / "folder/test.workflow.yaml") ==
      Right(WorkflowPath("/folder/test") -> SourceType.Yaml))
    assert(fileToTypedPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.workflow.txt") ==
      Right(WorkflowPath("/folder/test") -> SourceType.Txt))
    assert(fileToTypedPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.job_chain.xml") ==
      Right(WorkflowPath("/folder/test") -> SourceType.Xml))
    assert(fileToTypedPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.workflow.wrong") ==
      Left(Problem(s"File '...${separator}folder${separator}test.workflow.wrong' is not recognized as a configuration file")))
    assert(fileToTypedPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.workflow.json") ==
      Right(WorkflowPath("/folder/test") -> SourceType.Json))
    assert(fileToTypedPathAndSourceType(Set(WorkflowPath), dir, dir / "a@b.workflow.json") ==
      Left(InvalidNameProblem("WorkflowPath", "a@b")))
  }

  if (sys.props contains "test.speed") "speed" in {
    val dir = Paths.get("/TEST/JS7/PROVIDER/CONFIG/LIVE")
    val path = dir / "folder/test.workflow.json"
    for (_ <- 1 to 5) info(
      measureTime(100000, "fileToTypedPathAndSourceType") {
      fileToTypedPathAndSourceType(Set(WorkflowPath), dir, path)
    }.toString)
  }
}
