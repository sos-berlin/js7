package com.sos.jobscheduler.master.agent

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.common.http.CirceToYaml.ToYamlString
import com.sos.jobscheduler.common.scalautil.FileUtils
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.master.agent.AgentReaderTest._
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class AgentReaderTest extends FreeSpec {

  "Different Agent file formats" in {
    FileUtils.withTemporaryDirectory("AgentReaderTest-") { dir ⇒
      val expected = mutable.Buffer[Agent]()

      // JSON
      val jsonAgent = Agent(AgentPath("/JSON") % TestVersionId, "https://JSON")
      (dir / "JSON.agent.json").contentString = jsonAgent.withoutId.asJson.toPrettyString
      expected += jsonAgent

      // YAML
      val yamlAgent = Agent(AgentPath("/YAML") % TestVersionId, "https://JSON")
      (dir / "YAML.agent.yaml").contentString = yamlAgent.asJson.toYamlString
      expected += yamlAgent

      // XML
      val xmlAgent = Agent(AgentPath("/XML") % TestVersionId, "https://XML")
      (dir / "XML.agent.xml").xml = <agent uri="https://XML"/>
      expected += xmlAgent

      assert(FileBasedReader.readDirectoryTree(AgentReader :: Nil, dir, TestVersionId).map(_.toSet) ==
        Valid(expected.toSet))
    }
  }
}

object AgentReaderTest {
  private val TestVersionId = VersionId("1.0.0")
}
