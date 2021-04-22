package js7.controller.workflow

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.problem.Checked.Ops
import js7.common.http.CirceToYaml.ToYamlString
import js7.core.item.TypedSourceReader
import js7.data.agent.AgentPath
import js7.data.job.RelativePathExecutable
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class WorkflowReaderTest extends AnyFreeSpec {

  "Different Workflow file formats" in {
    withTemporaryDirectory("WorkflowReaderTest-") { dir =>
      val expected = mutable.Buffer[Workflow]()

      // JSON
      val jsonWorkflow = Workflow.of(Execute(WorkflowJob(AgentPath("AGENT"), RelativePathExecutable("JSON.sh"))))
      dir / "JSON.workflow.json" := jsonWorkflow.asJson.toPrettyString
      expected += jsonWorkflow.withId(WorkflowPath("JSON"))

      // YAML
      val yamlWorkflow = Workflow.of(Execute(WorkflowJob(AgentPath("AGENT"), RelativePathExecutable("YAML.sh"))))
      dir / "YAML.workflow.yaml" := yamlWorkflow.asJson.toYamlString
      expected += yamlWorkflow.withId(WorkflowPath("YAML"))

      // SCRIPT
      val script = """define workflow { execute executable="TEST.sh", agent="AGENT"; }"""
      dir / "TXT.workflow.txt" := script
      expected += WorkflowParser.parse(script).orThrow.withId(WorkflowPath("TXT"))

      val typedSourceReader = new TypedSourceReader(dir, WorkflowReader :: Nil)
      assert(typedSourceReader.readCompleteDirectory().map(_.toSet) == Right(expected.toSet))
    }
  }
}
