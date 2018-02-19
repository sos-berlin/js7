package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.core.filebased.TypedPathDirectoryWalker.{TypedFile, checkUniqueness, typedFiles}
import com.sos.jobscheduler.core.filebased.TypedPathDirectoryWalkerTest._
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.workflow.{JobPath, WorkflowPath}
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.Path
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TypedPathDirectoryWalkerTest extends FreeSpec {

  "typedFiles, checkUniqueness" in {
    provideDataDirectory { dir ⇒
      val checkedTypedFiles = typedFiles(dir, Set(JobPath, WorkflowPath))
      assert(checkedTypedFiles.toSet == Set(
        Valid(TypedFile(dir / "test.job.xml", AJobPath, SourceType.Xml)),
        Valid(TypedFile(dir / "test.workflow.json", AWorkflowPath, SourceType.Json)),
        Valid(TypedFile(dir / "test.workflow.txt", AWorkflowPath, SourceType.Txt)),
        Valid(TypedFile(dir / "test.job_chain.xml", AWorkflowPath, SourceType.Xml)),
        Valid(TypedFile(dir / "folder" / "test.job.xml", BJobPath, SourceType.Xml)),
        Invalid(Problem("File 'folder/test.alien.xml' is not recognized as a configuration file"))))
      assert(checkUniqueness(checkedTypedFiles) == Invalid(Problem(
        s"Duplicate configuration files: ${dir / "test.job_chain.xml"}, ${dir / "test.workflow.json"}, ${dir / "test.workflow.txt"}")))
    }
  }
}

object TypedPathDirectoryWalkerTest {
  private val AJobPath = JobPath("/test")
  private val BJobPath = JobPath("/folder/test")
  private val AWorkflowPath = WorkflowPath("/test")

  private def provideDataDirectory[A](body: Path ⇒ A): A = {
    val dir = createTempDirectory("test-")
    val subdir = dir / "folder"
    createDirectories(subdir)
    createDirectories(dir / "ignored.job.xml")
    (dir / "test.job.xml").xml = <job/>
    (dir / "test.workflow.json").contentString = "{}"
    (dir / "test.workflow.txt").contentString = ""
    (dir / "test.job_chain.xml").xml = <job_chain/>
    (subdir / "test.job.xml").xml = <job/>
    (subdir / "test.alien.xml").xml = <ignored/>
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }
}
