package com.sos.jobscheduler.master.tests

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.core.filebased.FileBaseds
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, VersionAdded}
import com.sos.jobscheduler.data.filebased.{FileBased, TypedPath, VersionId}
import com.sos.jobscheduler.data.workflow.instructions.ExplicitEnd
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.agent.AgentReader
import com.sos.jobscheduler.master.scheduledorder.ScheduledOrderGeneratorReader
import com.sos.jobscheduler.master.tests.FileBasedsTest._
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
      // We assume an existing version V0
      val v0FileBaseds = List(AWorkflow, BWorkflow, DWorkflow, AAgent)

      // Write folder image for version V1, using different source types
      (directory / "A.workflow.json").contentString = AWorkflow.asJson.toPrettyString  // Same
      (directory / "C.workflow.txt").contentString = CWorkflow.source.get              // Same
      (directory / "D.workflow.txt").contentString = D1Workflow.source.get             // Changed
      (directory / "A.agent.xml").xml = <agent uri="http://A"/>                        // Same
      (directory / "folder" / "B.agent.xml").xml = <agent uri="http://B"/>             // Added

      val eventsChecked = FileBaseds.readDirectory(readers, directory, v0FileBaseds, V1)
      assert(eventsChecked.map(_.toSet) == Valid(Set(
        VersionAdded(V1),
        FileBasedDeleted(BWorkflow.path),
        FileBasedAdded(BAgent withVersion V1),
        FileBasedAdded(CWorkflow withVersion V1),
        FileBasedChanged(D1Workflow withVersion V1))))
    }
  }

  "Diff" in {
    val diff = FileBaseds.Diff.fromEvents(
      List(
        FileBasedDeleted(BWorkflow.path),
        FileBasedAdded(BAgent),
        FileBasedAdded(CWorkflow),
        FileBasedChanged(D1Workflow)))

    assert(diff == FileBaseds.Diff[TypedPath, FileBased](
      List(BAgent, CWorkflow),
      List(D1Workflow),
      List(BWorkflow.path)))

    assert(diff.select[WorkflowPath, Workflow] ==
      FileBaseds.Diff[WorkflowPath, Workflow](
        List(CWorkflow),
        List(D1Workflow),
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

  private[tests] val V0 = VersionId("0")
  private[tests] val V1 = VersionId("1")
  private val u = VersionId("UNKNOWN")

  private[tests] val AWorkflow = Workflow.of(WorkflowPath("/A") % u)
  private[tests] val BWorkflow = Workflow(WorkflowPath("/B") % u, Vector("B-END" @: ExplicitEnd))
  private[tests] val CWorkflow = WorkflowParser.parse(WorkflowPath("/C") % u, "// EMPTY").orThrow
  private[tests] val DWorkflow = Workflow(WorkflowPath("/D") % u, Vector("D-END" @: ExplicitEnd))
  private[tests] val D1Workflow = WorkflowParser.parse(WorkflowPath("/D") % u, "CHANGED-D-END: end").orThrow
  private[tests] val AAgent = Agent(AgentPath("/A") % u, "http://A")
  private[tests] val BAgent = Agent(AgentPath("/folder/B") % u, "http://B")


  private[tests] def provideDirectory[A](body: Path ⇒ A): A = {
    val dir = createTempDirectory("test-")
    val folder = dir / "folder"
    createDirectories(folder)
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }
}
