package com.sos.jobscheduler.master.tests

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.core.filebased.FileBasedReader.{readDirectoryTree, readDirectoryTreeWithProblems}
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.master.agent.AgentReader
import com.sos.jobscheduler.master.tests.FileBasedReaderTest._
import com.sos.jobscheduler.master.tests.FileBasedsTest.{AAgent, AWorkflow, BAgent, BWorkflow, CWorkflow, V0, provideDirectory}
import com.sos.jobscheduler.master.workflow.WorkflowReader
import io.circe.syntax.EncoderOps
import java.io.File.separator
import java.nio.file.Files.createDirectories
import java.nio.file.Path
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedReaderTest extends FreeSpec {

  "readDirectoryTreeWithProblems" in {
    provideDirectory { directory ⇒
      createFiles(directory)
      val result = readDirectoryTreeWithProblems(Set(WorkflowReader, AgentReader), directory, V0).orThrow
      assert(result.map(_.mapProblem(o ⇒ Problem(o.toString))).toSet == Set(
        Valid(AWorkflow withVersion V0),
        Valid(BWorkflow withVersion V0),
        Valid(CWorkflow withVersion V0),
        Invalid(Problem("""Problem with 'Workflow:/D' (txt) [End:1:1 ..."ERROR"]""")),
        Invalid(Problem("""Problem with 'Workflow:/E' (JSON) [expected json value got N (line 1, column 1)]""")),
        Invalid(Problem(s"File '...${separator}folder${separator}test.alien.json' is not recognized as a configuration file")),
        Valid(AAgent withVersion V0),
        Valid(BAgent withVersion V0)))

      assert(readDirectoryTree(Set(WorkflowReader, AgentReader), directory, VersionId("VERSION")) ==
        Invalid(Problem.set(
          """Problem with 'Workflow:/D' (txt) [End:1:1 ..."ERROR"]""",
          """Problem with 'Workflow:/E' (JSON) [expected json value got N (line 1, column 1)]""",
         s"""File '...${separator}folder${separator}test.alien.json' is not recognized as a configuration file""")))

      (directory / "A.workflow.txt").contentString = ""
      assert(readDirectoryTreeWithProblems(Set(WorkflowReader, AgentReader), directory, VersionId("VERSION")) ==
        Invalid(Problem(s"Duplicate configuration files: ${directory / "A.workflow.json"}, ${directory / "A.workflow.txt"}")))
    }
  }
}

object FileBasedReaderTest {
  private def createFiles(directory: Path): Unit = {
    createDirectories(directory / "ignored.agent.xml")  // Empty directory named like an agent is ignored
    (directory / "A.workflow.json").contentString = AWorkflow.withoutId.asJson.toPrettyString
    (directory / "B.job_chain.xml").xml = <job_chain><job_chain_node.end state="B-END"/></job_chain>
    (directory / "C.workflow.txt").contentString = "// EMPTY"
    (directory / "D.workflow.txt").contentString = "ERROR"
    (directory / "E.workflow.json").contentString = "NO-JSON"
    (directory / "A.agent.json").contentString = """{ "uri": "http://A" }"""
    (directory / "folder" / "B.agent.xml").xml = <agent uri="http://B"/>
    (directory / "folder" / "test.alien.json").contentString = ""
  }
}
