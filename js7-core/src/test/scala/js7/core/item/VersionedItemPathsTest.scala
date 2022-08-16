package js7.core.item

import java.io.File.separator
import java.nio.file.Paths
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Problem
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.test.Test
import js7.base.time.Stopwatch.measureTime
import js7.core.item.ItemPaths.*
import js7.data.item.SourceType
import js7.data.workflow.WorkflowPath

/**
  * @author Joacim Zschimmer
  */
final class VersionedItemPathsTest extends Test {

  "fileToItemPathAndSourceType" in {
    val dir = Paths.get("DIR")
    assert(fileToItemPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.workflow.json") ==
      Right(WorkflowPath("folder/test") -> SourceType.Json))
    //assert(fileToItemPathAndSourceType(Set(WorkflowPath, AgentPath), dir, dir / "folder/test.workflow.json") ==
    //  Right(WorkflowPath("folder/test") -> SourceType.Json))
    assert(fileToItemPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.workflow.txt") ==
      Right(WorkflowPath("folder/test") -> SourceType.Txt))
    assert(fileToItemPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.workflow.wrong") ==
      Left(Problem(s"File '...${separator}folder${separator}test.workflow.wrong'" +
        " is not recognized as a configuration file" +
        " (like *.workflow.json, *.workflow.txt)")))
    assert(fileToItemPathAndSourceType(Set(WorkflowPath), dir, dir / "folder/test.workflow.json") ==
      Right(WorkflowPath("folder/test") -> SourceType.Json))
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
