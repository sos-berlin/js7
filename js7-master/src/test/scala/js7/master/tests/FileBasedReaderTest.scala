package js7.master.tests

import io.circe.syntax.EncoderOps
import java.io.File.separator
import java.nio.file.Files.{createDirectories, createDirectory, createTempDirectory, delete}
import js7.base.problem.Problem
import js7.common.files.DirectoryReader
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import js7.core.filebased.TypedSourceReader
import js7.master.agent.AgentRefReader
import js7.master.tests.FileBasedsTest.{AAgent, AWorkflow, BAgent, BWorkflow, CWorkflow}
import js7.master.workflow.WorkflowReader
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedReaderTest extends AnyFreeSpec with BeforeAndAfterAll
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
        Left(Problem.Combined(Set(
          Problem("""Problem with 'Workflow:/D' (txt) [Expected "define":1:1, found "ERROR"]"""),
          Problem("""Problem with 'Workflow:/E' (JSON) [JSON ParsingFailure: expected json value got 'NO-JSO...' (line 1, column 1)]"""),
          Problem(s"File '...${separator}folder${separator}test.alien.json' is not recognized as a configuration file")))))
  }

  "Duplicate FileBased, Workflows are not checked" in {
    directory / "A.workflow.txt" := "DUPLICATE"
    assert(typedSourceReader.readFileBaseds(DirectoryReader.files(directory)) ==
      Left(Problem.Combined(Set(
        Problem(s"Duplicate configuration files: ${directory / "A.workflow.json"}, ${directory / "A.workflow.txt"}"),
        Problem(s"File '...${separator}folder${separator}test.alien.json' is not recognized as a configuration file")))))
  }

  "Only valid FileBased" in {
    delete(directory / "A.workflow.txt")
    delete(directory / "D.workflow.txt")
    delete(directory / "E.workflow.json")
    delete(directory / "folder/test.alien.json")
    assert(typedSourceReader.readFileBaseds(DirectoryReader.files(directory)).map(_.toSet)
      == Right(Set(AWorkflow, BWorkflow, CWorkflow, AAgent, BAgent)))
  }
}
