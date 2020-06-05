package js7.master.agent

import js7.base.circeutils.CirceUtils.RichJson
import js7.base.web.Uri
import js7.common.http.CirceToYaml.ToYamlString
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.withTemporaryDirectory
import js7.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import js7.core.filebased.TypedSourceReader
import js7.data.agent.{AgentRef, AgentRefPath}
import io.circe.syntax.EncoderOps
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class AgentRefReaderTest extends AnyFreeSpec {

  "Different AgentRef file formats" in {
    withTemporaryDirectory("AgentRefReaderTest-") { dir =>
      val expected = mutable.Buffer[AgentRef]()

      // JSON
      val jsonAgent = AgentRef(AgentRefPath("/JSON"), Uri("https://JSON"))
      (dir / "JSON.agentref.json") := jsonAgent.asJson.toPrettyString
      expected += jsonAgent

      // YAML
      val yamlAgent = AgentRef(AgentRefPath("/YAML"), Uri("https://JSON"))
      (dir / "YAML.agentref.yaml") := yamlAgent.asJson.toYamlString
      expected += yamlAgent

      // XML
      val xmlAgent = AgentRef(AgentRefPath("/XML"), Uri("https://XML"))
      (dir / "XML.agent.xml").xml = <agent uri="https://XML"/>
      expected += xmlAgent

      val typedSourceReader = new TypedSourceReader(dir, AgentRefReader :: Nil)
      assert(typedSourceReader.readCompleteDirectory().map(_.toSet) == Right(expected.toSet))
    }
  }
}
