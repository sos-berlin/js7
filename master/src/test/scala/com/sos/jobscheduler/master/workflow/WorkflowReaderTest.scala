package com.sos.jobscheduler.master.workflow

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.http.CirceToYaml.ToYamlString
import com.sos.jobscheduler.common.scalautil.FileUtils
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.core.filebased.TypedSourceReader
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class WorkflowReaderTest extends FreeSpec {

  "Different Workflow file formats" in {
    FileUtils.withTemporaryDirectory("WorkflowReaderTest-") { dir =>
      val expected = mutable.Buffer[Workflow]()

      // JSON
      val jsonWorkflow = Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/JSON.sh"))))
      dir / "JSON.workflow.json" := jsonWorkflow.asJson.toPrettyString
      expected += jsonWorkflow.withId(WorkflowPath("/JSON"))

      // YAML
      val yamlWorkflow = Workflow.of(Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/YAML.sh"))))
      dir / "YAML.workflow.yaml" := yamlWorkflow.asJson.toYamlString
      expected += yamlWorkflow.withId(WorkflowPath("/YAML"))

      // SCRIPT
      val script = """define workflow { execute executable="/TEST.sh", agent="/AGENT"; }"""
      dir / "TXT.workflow.txt" := script
      expected += WorkflowParser.parse(script).orThrow.withId(WorkflowPath("/TXT"))

      val typedSourceReader = new TypedSourceReader(dir, WorkflowReader :: Nil)
      assert(typedSourceReader.readCompleteDirectory().map(_.toSet) == Valid(expected.toSet))
    }
  }
}
