package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.files.DirectoryReader
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.core.filebased.TypedFile.checkUniqueness
import com.sos.jobscheduler.core.filebased.TypedFileTest._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.workflow.WorkflowPath
import java.io.File.separator
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.Path
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TypedFileTest extends FreeSpec
{
  "typedFiles, checkUniqueness" in {
    provideDataDirectory { dir =>
      val checkedTypedFiles = DirectoryReader.files(dir).map(TypedFile.checked(dir, _, Set(AgentRefPath, WorkflowPath)))
      assert(checkedTypedFiles.toSet == Set(
        Right(TypedFile(dir / "test.agentref.json", AAgentRefPath, SourceType.Json)),
        Right(TypedFile(dir / "test.workflow.json", AWorkflowPath, SourceType.Json)),
        Right(TypedFile(dir / "test.workflow.txt", AWorkflowPath, SourceType.Txt)),
        Right(TypedFile(dir / "folder/test.agentref.json", BAgentRefPath, SourceType.Json)),
        Left(Problem(s"File '...${separator}folder${separator}test.alien.json' is not recognized as a configuration file"))))
      assert(checkUniqueness(checkedTypedFiles collect { case Right(o) => o }) == Left(Problem(
        s"Duplicate configuration files: ${dir / "test.workflow.json"}, ${dir / "test.workflow.txt"}")))
    }
  }
}

object TypedFileTest
{
  private val AAgentRefPath = AgentRefPath("/test")
  private val BAgentRefPath = AgentRefPath("/folder/test")
  private val AWorkflowPath = WorkflowPath("/test")

  private def provideDataDirectory[A](body: Path => A): A = {
    val dir = createTempDirectory("test-")
    val subdir = dir / "folder"
    createDirectories(subdir)
    createDirectories(dir / "ignored.job.xml")
    (dir / "test.agentref.json") := "{}"
    (dir / "test.workflow.json") := "{}"
    (dir / "test.workflow.txt") := ""
    (subdir / "test.agentref.json") := "{}"
    (subdir / "test.alien.json") := "{}"
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }

}
