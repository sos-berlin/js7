package com.sos.jobscheduler.master.fileBased

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.core.filebased.FileBaseds
import com.sos.jobscheduler.core.workflow.notation.WorkflowParser
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, VersionAdded}
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedVersion, TypedPath}
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
      (directory / "A.workflow.json").contentString = AWorkflow.asJson.toPrettyString
      (directory / "C.workflow.txt").contentString = CWorkflow.source.get
      (directory / "D.workflow.txt").contentString = ChangedDWorkflow.source.get
      (directory / "A.agent.xml").xml = <agent uri="http://A"/>
      (directory / "folder" / "B.agent.xml").xml = <agent uri="http://B"/>

      val previousFileBaseds = List(AWorkflow, BWorkflow, DWorkflow, AAgent)
      val eventsChecked = FileBaseds.readDirectory(readers, directory, previousFileBaseds, FileBasedVersion("VERSION"))
      assert(eventsChecked.map(_.toSet) == Valid(Set(
        VersionAdded(FileBasedVersion("VERSION")),
        FileBasedDeleted(BWorkflow.path),
        FileBasedAdded(BAgent),
        FileBasedAdded(CWorkflow),
        FileBasedChanged(ChangedDWorkflow))))
    }
  }

  "Diff" in {
    val diff = FileBaseds.Diff.fromEvents(
      List(
        FileBasedDeleted(BWorkflow.path),
        FileBasedAdded(BAgent),
        FileBasedAdded(CWorkflow),
        FileBasedChanged(ChangedDWorkflow)))

    assert(diff == FileBaseds.Diff[TypedPath, FileBased](
      List(BAgent, CWorkflow),
      List(ChangedDWorkflow),
      List(BWorkflow.path)))

    assert(diff.select[WorkflowPath, Workflow] ==
      FileBaseds.Diff[WorkflowPath, Workflow](
        List(CWorkflow),
        List(ChangedDWorkflow),
        List(BWorkflow.path)))

    assert(diff.select[AgentPath, Agent] ==
      FileBaseds.Diff[AgentPath, Agent](
        List(BAgent),
        Nil,
        Nil))
  }
}

object FileBasedsTest {
  private val readers = Set(WorkflowReader, AgentReader, new ScheduledOrderGeneratorReader(ZoneId.of("UTC")))
  private[fileBased] val AWorkflow = Workflow.of(WorkflowPath("/A"))
  private[fileBased] val BWorkflow = Workflow(WorkflowPath("/B"), Vector("B-END" @: ExplicitEnd))
  private[fileBased] val CWorkflow = WorkflowParser.parse(WorkflowPath("/C"), "// EMPTY").force
  private[fileBased] val DWorkflow = Workflow(WorkflowPath("/D"), Vector("D-END" @: ExplicitEnd))
  private[fileBased] val ChangedDWorkflow = WorkflowParser.parse(WorkflowPath("/D"), "CHANGED-D-END: end").force
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
