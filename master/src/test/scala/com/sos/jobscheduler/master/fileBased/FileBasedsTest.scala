package com.sos.jobscheduler.master.fileBased

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.core.filebased.FileBaseds.readDirectory
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.FileBasedEvent.FileBasedAdded
import com.sos.jobscheduler.data.workflow.instructions.ExplicitEnd
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.agent.AgentReader
import com.sos.jobscheduler.master.fileBased.FileBasedsTest._
import com.sos.jobscheduler.master.order.ScheduledOrderGeneratorReader
import com.sos.jobscheduler.master.order.agent.Agent
import com.sos.jobscheduler.master.workflow.WorkflowReader
import io.circe.syntax.EncoderOps
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.Path
import java.time.ZoneId
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedsTest extends FreeSpec {

  "readDirectory" in {
    provideDirectory { directory ⇒
      (directory / "A.workflow.json").contentString = ANamedWorkflow.workflow.asJson.toPrettyString
      (directory / "C.workflow.txt").contentString = "// EMPTY"
      (directory / "A.agent.xml").xml = <agent uri="http://A"/>
      (directory / "folder" / "B.agent.xml").xml = <agent uri="http://B"/>
      val existingFileBaseds = List(ANamedWorkflow, BNamedWorkflow, AAgent)
      assert(readDirectory(directory, readers, existingFileBaseds).map(_.toSet) == Valid(Set(
        FileBasedAdded(CNamedWorkflow),
        FileBasedAdded(BAgent))))
    }
  }
}

object FileBasedsTest {
  private val readers = Set(WorkflowReader, AgentReader, new ScheduledOrderGeneratorReader(ZoneId.of("UTC")))
  private[fileBased] val ANamedWorkflow = Workflow.Named(WorkflowPath("/A"), Workflow.of())
  private[fileBased] val BNamedWorkflow = Workflow.Named(WorkflowPath("/B"), Workflow(Vector("END" @: ExplicitEnd)))
  private[fileBased] val CNamedWorkflow = Workflow.Named(WorkflowPath("/C"), Workflow.of().copy(source = Some("// EMPTY")))
  private[fileBased] val AAgent = Agent(AgentPath("/A"), "http://A")
  private[fileBased] val BAgent = Agent(AgentPath("/folder/B"), "http://B")

  private[fileBased] def provideDirectory[A](body: Path ⇒ A): A = {
    val dir = createTempDirectory("test-")
    val folder = dir / "folder"
    createDirectories(folder)
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }
}
