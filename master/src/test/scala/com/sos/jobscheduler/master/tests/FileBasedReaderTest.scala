package com.sos.jobscheduler.master.tests

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.files.DirectoryReader
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.core.filebased.TypedSourceReader
import com.sos.jobscheduler.master.agent.AgentRefReader
import com.sos.jobscheduler.master.tests.FileBasedsTest.{AAgent, AWorkflow, BAgent, BWorkflow, CWorkflow}
import com.sos.jobscheduler.master.workflow.WorkflowReader
import io.circe.syntax.EncoderOps
import java.io.File.separator
import java.nio.file.Files.{createDirectories, createDirectory, createTempDirectory, delete}
import org.scalatest.{BeforeAndAfterAll, FreeSpec}

/**
  * @author Joacim Zschimmer
  */
final class FileBasedReaderTest extends FreeSpec with BeforeAndAfterAll
{
  private lazy val directory = createTempDirectory("test-")

  override def beforeAll() = {
    super.beforeAll()
    createDirectory(directory / "folder")
    createDirectories(directory / "ignored.agent.xml")  // Empty directory named like an agent is ignored
    (directory / "A.workflow.json") := AWorkflow.withoutId.asJson
    (directory / "B.workflow.json") := BWorkflow.withoutId.asJson
    (directory / "C.workflow.txt") := "define workflow { /*EMPTY*/ }"
    (directory / "D.workflow.txt") := "ERROR"
    (directory / "E.workflow.json") := "NO-JSON"
    (directory / "A.agentref.json") := """{ "uri": "http://A" }"""
    (directory / "folder" / "B.agent.xml").xml = <agent uri="http://B"/>
    (directory / "folder" / "test.alien.json") := ""
  }

  override def afterAll() = {
    deleteDirectoryRecursively(directory)
    super.afterAll()
  }

  private lazy val typedSourceReader = new TypedSourceReader(directory, Set(WorkflowReader, AgentRefReader))

  "readFileBased with syntax errors and an alien file" in {
      assert(typedSourceReader.readFileBaseds(DirectoryReader.files(directory)) ==
        Invalid(Problem.Multiple(Set(
          Problem("""Problem with 'Workflow:/D' (txt) ["define":1:1 ..."ERROR"]"""),
          Problem("""Problem with 'Workflow:/E' (JSON) [expected json value got 'NO-JSO...' (line 1, column 1)]"""),
          Problem(s"File '...${separator}folder${separator}test.alien.json' is not recognized as a configuration file")))))
  }

  "Duplicate FileBased, Workflows are not checked" in {
    directory / "A.workflow.txt" := "DUPLICATE"
    assert(typedSourceReader.readFileBaseds(DirectoryReader.files(directory)) ==
      Invalid(Problem.Multiple(Set(
        Problem(s"Duplicate configuration files: ${directory / "A.workflow.json"}, ${directory / "A.workflow.txt"}"),
        Problem(s"File '...${separator}folder${separator}test.alien.json' is not recognized as a configuration file")))))
  }

  "Only valid FileBased" in {
    delete(directory / "A.workflow.txt")
    delete(directory / "D.workflow.txt")
    delete(directory / "E.workflow.json")
    delete(directory / "folder/test.alien.json")
    assert(typedSourceReader.readFileBaseds(DirectoryReader.files(directory)).map(_.toSet)
      == Valid(Set(AWorkflow, BWorkflow, CWorkflow, AAgent, BAgent)))
  }
}
