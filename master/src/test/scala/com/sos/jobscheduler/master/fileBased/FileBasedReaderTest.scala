package com.sos.jobscheduler.master.fileBased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.problem.Checked.ops.RichChecked
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.core.filebased.FileBasedReader.{readDirectoryTree, readDirectoryTreeFlattenProblems}
import com.sos.jobscheduler.master.agent.AgentReader
import com.sos.jobscheduler.master.fileBased.FileBasedReaderTest._
import com.sos.jobscheduler.master.fileBased.FileBasedsTest.{AAgent, ANamedWorkflow, BAgent, BNamedWorkflow, CNamedWorkflow, provideDirectory}
import com.sos.jobscheduler.master.workflow.WorkflowReader
import io.circe.syntax.EncoderOps
import java.nio.file.Files.createDirectories
import java.nio.file.Path
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedReaderTest extends FreeSpec {

  "readDirectoryTree" in {
    provideDirectory { directory ⇒
      createFiles(directory)
      assert(readDirectoryTree(directory, Set(WorkflowReader, AgentReader)).force.toSet == Set(
        Valid(ANamedWorkflow),
        Valid(BNamedWorkflow),
        Valid(CNamedWorkflow),
        Invalid(Problem("""Problem with 'Workflow:/D (txt)': Failure(End:1:1 ..."ERROR")""")),
        Invalid(Problem("""File 'folder/test.alien.json' is not recognized as a configuration file""")),
        Valid(AAgent),
        Valid(BAgent)))

      assert(readDirectoryTreeFlattenProblems(directory, Set(WorkflowReader, AgentReader)) ==
        Invalid(Problem.set(
          """Problem with 'Workflow:/D (txt)': Failure(End:1:1 ..."ERROR")""",
          """File 'folder/test.alien.json' is not recognized as a configuration file""")))

      (directory / "A.workflow.txt").contentString = ""
      assert(readDirectoryTree(directory, Set(WorkflowReader, AgentReader)) ==
        Invalid(Problem(s"Duplicate configuration files: ${directory / "A.workflow.json"}, ${directory / "A.workflow.txt"}")))
    }
  }
}

object FileBasedReaderTest {
  private def createFiles(directory: Path): Unit = {
    createDirectories(directory / "ignored.agent.xml")  // Empty directory named like an agent is ignored
    (directory / "A.workflow.json").contentString = ANamedWorkflow.workflow.asJson.toPrettyString
    (directory / "B.job_chain.xml").xml = <job_chain><job_chain_node.end state="END"/></job_chain>
    (directory / "C.workflow.txt").contentString = "// EMPTY"
    (directory / "D.workflow.txt").contentString = "ERROR"
    (directory / "A.agent.xml").xml = <agent uri="http://A"/>
    (directory / "folder" / "B.agent.xml").xml = <agent uri="http://B"/>
    (directory / "folder" / "test.alien.json").contentString = ""
  }
}
