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
      (directory / "A.workflow.json").contentString = ANamedWorkflow.workflow.asJson.toPrettyString
      (directory / "C.workflow.txt").contentString = CNamedWorkflow.workflow.source.get
      (directory / "D.workflow.txt").contentString = ChangedDNamedWorkflow.workflow.source.get
      (directory / "A.agent.xml").xml = <agent uri="http://A"/>
      (directory / "folder" / "B.agent.xml").xml = <agent uri="http://B"/>

      val previousFileBaseds = List(ANamedWorkflow, BNamedWorkflow, DNamedWorkflow, AAgent)
      val eventsChecked = FileBaseds.readDirectory(readers, directory, previousFileBaseds, FileBasedVersion("VERSION"))
      assert(eventsChecked.map(_.toSet) == Valid(Set(
        VersionAdded(FileBasedVersion("VERSION")),
        FileBasedDeleted(BNamedWorkflow.path),
        FileBasedAdded(BAgent),
        FileBasedAdded(CNamedWorkflow),
        FileBasedChanged(ChangedDNamedWorkflow))))
    }
  }

  "Diff" in {
    val diff = FileBaseds.Diff.fromEvents(
      List(
        FileBasedDeleted(BNamedWorkflow.path),
        FileBasedAdded(BAgent),
        FileBasedAdded(CNamedWorkflow),
        FileBasedChanged(ChangedDNamedWorkflow)))

    assert(diff == FileBaseds.Diff[TypedPath, FileBased](
      List(BAgent, CNamedWorkflow),
      List(ChangedDNamedWorkflow),
      List(BNamedWorkflow.path)))

    assert(diff.select[WorkflowPath, Workflow.Named] ==
      FileBaseds.Diff[WorkflowPath, Workflow.Named](
        List(CNamedWorkflow),
        List(ChangedDNamedWorkflow),
        List(BNamedWorkflow.path)))

    assert(diff.select[AgentPath, Agent] ==
      FileBaseds.Diff[AgentPath, Agent](
        List(BAgent),
        Nil,
        Nil))
  }
}

object FileBasedsTest {
  private val readers = Set(WorkflowReader, AgentReader, new ScheduledOrderGeneratorReader(ZoneId.of("UTC")))
  private[fileBased] val ANamedWorkflow = Workflow.Named(WorkflowPath("/A"), Workflow.of())
  private[fileBased] val BNamedWorkflow = Workflow.Named(WorkflowPath("/B"), Workflow(Vector("B-END" @: ExplicitEnd)))
  private[fileBased] val CNamedWorkflow = Workflow.Named(WorkflowPath("/C"), WorkflowParser.parse("// EMPTY").force)
  private[fileBased] val DNamedWorkflow = Workflow.Named(WorkflowPath("/D"), Workflow(Vector("D-END" @: ExplicitEnd)))
  private[fileBased] val ChangedDNamedWorkflow = Workflow.Named(WorkflowPath("/D"), WorkflowParser.parse("CHANGED-D-END: end").force)
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
