package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.files.DirectoryReader
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.core.filebased.TypedFile.checkUniqueness
import com.sos.jobscheduler.core.filebased.TypedFileTest._
import com.sos.jobscheduler.data.agent.AgentPath
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
    provideDataDirectory { dir ⇒
      val checkedTypedFiles = DirectoryReader.files(dir).map(TypedFile.checked(dir, _, Set(AgentPath, WorkflowPath)))
      assert(checkedTypedFiles.toSet == Set(
        Valid(TypedFile(dir / "test.agent.json", AAgentPath, SourceType.Json)),
        Valid(TypedFile(dir / "test.workflow.json", AWorkflowPath, SourceType.Json)),
        Valid(TypedFile(dir / "test.workflow.txt", AWorkflowPath, SourceType.Txt)),
        Valid(TypedFile(dir / "folder/test.agent.json", BAgentPath, SourceType.Json)),
        Invalid(Problem(s"File '...${separator}folder${separator}test.alien.json' is not recognized as a configuration file"))))
      assert(checkUniqueness(checkedTypedFiles collect { case Valid(o) ⇒ o }) == Invalid(Problem(
        s"Duplicate configuration files: ${dir / "test.workflow.json"}, ${dir / "test.workflow.txt"}")))
    }
  }
}

object TypedFileTest
{
  private val AAgentPath = AgentPath("/test")
  private val BAgentPath = AgentPath("/folder/test")
  private val AWorkflowPath = WorkflowPath("/test")

  private def provideDataDirectory[A](body: Path ⇒ A): A = {
    val dir = createTempDirectory("test-")
    val subdir = dir / "folder"
    createDirectories(subdir)
    createDirectories(dir / "ignored.job.xml")
    (dir / "test.agent.json") := "{}"
    (dir / "test.workflow.json") := "{}"
    (dir / "test.workflow.txt") := ""
    (subdir / "test.agent.json") := "{}"
    (subdir / "test.alien.json") := "{}"
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }

}
