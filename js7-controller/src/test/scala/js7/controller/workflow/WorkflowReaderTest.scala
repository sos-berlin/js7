package js7.controller.workflow

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.problem.Checked.Ops
import js7.base.test.Test
import js7.core.item.TypedSourceReader
import js7.data.agent.AgentPath
import js7.data.job.RelativePathExecutable
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class WorkflowReaderTest extends Test {

  "Different Workflow file formats" in {
    withTemporaryDirectory("WorkflowReaderTest-") { dir =>
      val expected = mutable.Buffer[Workflow]()

      // JSON
      val jsonWorkflow = Workflow.of(Execute(WorkflowJob(AgentPath("AGENT"), RelativePathExecutable("JSON.sh"))))
      dir / "JSON.workflow.json" := jsonWorkflow.asJson.toPrettyString
      expected += jsonWorkflow.withId(WorkflowPath("JSON"))

      // SCRIPT
      val script = """define workflow { execute executable="TEST.sh", agent="AGENT"; }"""
      dir / "TXT.workflow.txt" := script
      expected += WorkflowParser.parse(script).orThrow.withId(WorkflowPath("TXT"))

      val typedSourceReader = new TypedSourceReader(dir, WorkflowReader :: Nil)
      assert(typedSourceReader.readCompleteDirectory().map(_.toSet) == Right(expected.toSet))
    }
  }
}
