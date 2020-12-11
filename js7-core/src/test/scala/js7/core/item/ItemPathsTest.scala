package js7.core.item

import java.io.File.separator
import java.nio.file.Paths
import js7.base.problem.Problem
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.time.Stopwatch.measureTime
import js7.common.scalautil.FileUtils.syntax._
import js7.core.item.ItemPaths._
import js7.data.item.SourceType
import js7.data.workflow.WorkflowPath
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ItemPathsTest extends AnyFreeSpec {

  "fileToItemPathAndSourceType" in {
    val dir = Paths.get("DIR")
    assert(fileToItemPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.workflow.json") ==
      Right(WorkflowPath("/folder/test") -> SourceType.Json))
    //assert(fileToItemPathAndSourceType(Set(WorkflowPath, AgentId), dir, dir / "folder/test.workflow.json") ==
    //  Right(WorkflowPath("/folder/test") -> SourceType.Json))
    //assert(fileToItemPathAndSourceType(Set(WorkflowPath, AgentId), dir, dir / "folder/test.workflow.yaml") ==
    //  Right(WorkflowPath("/folder/test") -> SourceType.Yaml))
    assert(fileToItemPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.workflow.txt") ==
      Right(WorkflowPath("/folder/test") -> SourceType.Txt))
    assert(fileToItemPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.job_chain.xml") ==
      Right(WorkflowPath("/folder/test") -> SourceType.Xml))
    assert(fileToItemPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.workflow.wrong") ==
      Left(Problem(s"File '...${separator}folder${separator}test.workflow.wrong' is not recognized as a configuration file")))
    assert(fileToItemPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.workflow.json") ==
      Right(WorkflowPath("/folder/test") -> SourceType.Json))
    assert(fileToItemPathAndSourceType(Set(WorkflowPath), dir, dir / "a@b.workflow.json") ==
      Left(InvalidNameProblem("WorkflowPath", "a@b")))
  }

  if (sys.props contains "test.speed") "speed" in {
    val dir = Paths.get("/TEST/JS7/PROVIDER/CONFIG/LIVE")
    val path = dir / "folder/test.workflow.json"
    for (_ <- 1 to 5) info(
      measureTime(100000, "fileToItemPathAndSourceType") {
      fileToItemPathAndSourceType(Set(WorkflowPath), dir, path)
    }.toString)
  }
}
