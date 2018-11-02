package com.sos.jobscheduler.master.workflow

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.http.CirceToYaml.ToYamlString
import com.sos.jobscheduler.common.scalautil.FileUtils
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.workflow.WorkflowReaderTest._
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class WorkflowReaderTest extends FreeSpec {

  "Different Workflow file formats" in {
    FileUtils.withTemporaryDirectory("WorkflowReaderTest-") { dir ⇒
      val expected = mutable.Buffer[Workflow]()

      // JSON
      val jsonWorkflow = Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/JSON.sh"))))
      (dir / "JSON.workflow.json").contentString = jsonWorkflow.asJson.toPrettyString
      expected += jsonWorkflow.withId(WorkflowPath("/JSON") % TestVersionId)

      // YAML
      val yamlWorkflow = Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/YAML.sh"))))
      (dir / "YAML.workflow.yaml").contentString = yamlWorkflow.asJson.toYamlString
      expected += yamlWorkflow.withId(WorkflowPath("/YAML") % TestVersionId)

      // SCRIPT
      val script = """workflow { execute executable="/TEST.sh", agent="/AGENT"; }"""
      (dir / "TXT.workflow.txt").contentString = script
      expected += WorkflowParser.parse(script).orThrow.withId(WorkflowPath("/TXT") % TestVersionId)

      assert(FileBasedReader.readDirectoryTree(WorkflowReader :: Nil, dir, TestVersionId).map(_.toSet) ==
        Valid(expected.toSet))
    }
  }
}

object WorkflowReaderTest {
  private val TestVersionId = VersionId("1.0.0")
}
