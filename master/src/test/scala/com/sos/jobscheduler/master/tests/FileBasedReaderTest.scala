package com.sos.jobscheduler.master.tests

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.problem.Checked.ops.RichChecked
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.core.filebased.FileBasedReader.readDirectoryTree
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.instructions.ExplicitEnd
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.agent.AgentReader
import com.sos.jobscheduler.master.order.agent.Agent
import com.sos.jobscheduler.master.tests.FileBasedReaderTest._
import com.sos.jobscheduler.master.workflow.WorkflowReader
import io.circe.syntax.EncoderOps
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.Path
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class FileBasedReaderTest extends FreeSpec {

  "FileBasedReader" in {
    provideDirectory { directory ⇒
      readDirectoryTree(Set(WorkflowReader, AgentReader), directory).force.toList should contain theSameElementsAs List(
        Valid(ANamedWorkflow),
        Valid(BNamedWorkflow),
        Valid(CNamedWorkflow),
        Invalid(Problem("""Problem with 'Workflow:/D (txt)': Failure(End:1:1 ..."ERROR")""")),
        Invalid(Problem("""File 'folder/test.alien.json' is not recognized as a configuration file""")),
        Valid(AAgent),
        Valid(BAgent))

      (directory / "A.workflow.txt").contentString = ""
      assert(readDirectoryTree(Set(WorkflowReader, AgentReader), directory) ==
        Invalid(Problem(s"Duplicate configuration files: $directory/A.workflow.json, $directory/A.workflow.txt")))
    }
  }
}

object FileBasedReaderTest {
  private val ANamedWorkflow = Workflow.Named(WorkflowPath("/A"), Workflow.of())
  private val BNamedWorkflow = Workflow.Named(WorkflowPath("/B"), Workflow(Vector("END" @: ExplicitEnd)))
  private val CNamedWorkflow = Workflow.Named(WorkflowPath("/C"), Workflow.of().copy(source = Some("// EMPTY")))
  private val AAgent = Agent(AgentPath("/A"), "http://A")
  private val BAgent = Agent(AgentPath("/folder/B"), "http://B")

  def provideDirectory[A](body: Path ⇒ A): A = {
    val dir = createTempDirectory("test-")
    val folder = dir / "folder"
    createDirectories(folder)
    createDirectories(dir / "ignored.agent.xml")  // Empty directory named like an agent is ignored
    (dir / "A.agent.xml").xml = <agent uri="http://A"/>
    (dir / "A.workflow.json").contentString = ANamedWorkflow.workflow.asJson.toPrettyString
    (dir / "B.job_chain.xml").xml = <job_chain><job_chain_node.end state="END"/></job_chain>
    (dir / "C.workflow.txt").contentString = "// EMPTY"
    (dir / "D.workflow.txt").contentString = "ERROR"
    (folder / "B.agent.xml").xml = <agent uri="http://B"/>
    (folder / "test.alien.json").contentString = ""
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }
}
