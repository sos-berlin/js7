package js7.core.item

import java.io.File.separator
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.Path
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Problem
import js7.common.files.DirectoryReader
import js7.core.item.VersionedItemFile.checkUniqueness
import js7.core.item.VersionedItemFileTest._
import js7.data.item.SourceType
import js7.data.workflow.WorkflowPath
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class VersionedItemFileTest extends AnyFreeSpec
{
  "typedFiles, checkUniqueness" in {
    provideDataDirectory { dir =>
      val checkedTypedFiles = DirectoryReader.files(dir).map(VersionedItemFile.checked(dir, _, Set(WorkflowPath)))
      assert(checkedTypedFiles.toSet == Set(
        Right(VersionedItemFile(dir / "test.workflow.json", AWorkflowPath, SourceType.Json)),
        Right(VersionedItemFile(dir / "test.workflow.txt", AWorkflowPath, SourceType.Txt)),
        Left(Problem(s"File '...${separator}folder${separator}test.alien.json' is not recognized as a configuration file"))))
      assert(checkUniqueness(checkedTypedFiles collect { case Right(o) => o }) == Left(Problem(
        s"Duplicate configuration files: ${dir / "test.workflow.json"}, ${dir / "test.workflow.txt"}")))
    }
  }
}

object VersionedItemFileTest
{
  private val AWorkflowPath = WorkflowPath("test")

  private def provideDataDirectory[A](body: Path => A): A = {
    val dir = createTempDirectory("test-")
    val subdir = dir / "folder"
    createDirectories(subdir)
    createDirectories(dir / "ignored.job.xml")
    (dir / "test.workflow.json") := "{}"
    (dir / "test.workflow.txt") := ""
    (subdir / "test.alien.json") := "{}"
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }
}
