package js7.controller.tests

import io.circe.syntax.EncoderOps
import java.io.File.separator
import java.nio.file.Files.{createDirectories, createDirectory, createTempDirectory, delete}
import js7.base.circeutils.CirceUtils.*
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.common.files.DirectoryReader
import js7.controller.tests.InventoryItemDiffTest.{ATestItem, AWorkflow, BTestItem, BWorkflow, CWorkflow, TestItemReader}
import js7.controller.workflow.WorkflowReader
import js7.core.item.TypedSourceReader
import org.scalatest.BeforeAndAfterAll

/**
  * @author Joacim Zschimmer
  */
final class VersionedItemReaderTest extends OurTestSuite, BeforeAndAfterAll:

  private lazy val directory = createTempDirectory("test-")

  override def beforeAll() =
    super.beforeAll()
    createDirectory(directory / "folder")
    createDirectories(directory / "ignored.agent.xml")  // Empty directory named like an agent is ignored
    (directory / "A.workflow.json") := AWorkflow.withoutId.asJson
    (directory / "B.workflow.json") := BWorkflow.withoutId.asJson
    (directory / "C.workflow.txt") := "define workflow { /*EMPTY*/ }"
    (directory / "D.workflow.txt") := "ERROR"
    (directory / "E.workflow.json") := "NO-JSON"
    (directory / "A.test.json") := json"""{ "content": "A" }"""
    (directory / "folder" / "B.test.json") := json"""{ "content": "B" }"""
    (directory / "folder" / "test.alien.json") := ""

  override def afterAll() =
    deleteDirectoryRecursively(directory)
    super.afterAll()

  private lazy val typedSourceReader = new TypedSourceReader(directory, Set(WorkflowReader, TestItemReader))

  "readVersionedItem with syntax errors and an alien file" in:
    assert(typedSourceReader.readItems(DirectoryReader.files(directory)) ==
      Left(Problem.Combined(Set(
        Problem(
          """Problem with 'Workflow:D' (txt) [Parsing failed at position 1 “❓ERROR” · Expected “define”]"""),
        Problem("""Problem with 'Workflow:E' (JSON) [JSON ParsingFailure: expected json value got 'NO-JSO...' (line 1, column 1)]"""),
        Problem(s"File '...${separator}folder${separator}test.alien.json'" +
          " is not recognized as a configuration file" +
          " (like *.workflow.json, *.workflow.txt, *.test.json)")))))

  "Duplicate VersionedItem, Workflows are not checked" in:
    directory / "A.workflow.txt" := "DUPLICATE"
    assert(typedSourceReader.readItems(DirectoryReader.files(directory)) ==
      Left(Problem.Combined(Set(
        Problem(s"Duplicate configuration files: ${directory / "A.workflow.json"}, ${directory / "A.workflow.txt"}"),
        Problem(s"File '...${separator}folder${separator}test.alien.json'" +
          " is not recognized as a configuration file" +
          " (like *.workflow.json, *.workflow.txt, *.test.json)")))))

  "Only valid VersionedItem" in:
    delete(directory / "A.workflow.txt")
    delete(directory / "D.workflow.txt")
    delete(directory / "E.workflow.json")
    delete(directory / "folder/test.alien.json")
    assert(typedSourceReader.readItems(DirectoryReader.files(directory)).map(_.toSet)
      == Right(Set(AWorkflow, BWorkflow, CWorkflow, ATestItem, BTestItem)))
