package com.sos.jobscheduler.master.agent

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.common.http.CirceToYaml.ToYamlString
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryDirectory
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.core.filebased.TypedSourceReader
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class AgentRefReaderTest extends FreeSpec {

  "Different AgentRef file formats" in {
    withTemporaryDirectory("AgentRefReaderTest-") { dir =>
      val expected = mutable.Buffer[AgentRef]()

      // JSON
      val jsonAgent = AgentRef(AgentRefPath("/JSON"), "https://JSON")
      (dir / "JSON.agentref.json") := jsonAgent.asJson.toPrettyString
      expected += jsonAgent

      // YAML
      val yamlAgent = AgentRef(AgentRefPath("/YAML"), "https://JSON")
      (dir / "YAML.agentref.yaml") := yamlAgent.asJson.toYamlString
      expected += yamlAgent

      // XML
      val xmlAgent = AgentRef(AgentRefPath("/XML"), "https://XML")
      (dir / "XML.agent.xml").xml = <agent uri="https://XML"/>
      expected += xmlAgent

      val typedSourceReader = new TypedSourceReader(dir, AgentRefReader :: Nil)
      assert(typedSourceReader.readCompleteDirectory().map(_.toSet) == Valid(expected.toSet))
    }
  }
}
