package com.sos.jobscheduler.agent.scheduler.job

import cats.data.Validated.Valid
import com.sos.jobscheduler.agent.scheduler.job.JobReaderTest._
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.common.http.CirceToYaml.ToYamlString
import com.sos.jobscheduler.common.scalautil.FileUtils
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.core.filebased.FileBasedReader
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.JobPath
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class JobReaderTest extends FreeSpec {

  "Different Job file formats" in {
    FileUtils.withTemporaryDirectory("JobReaderTest-") { dir ⇒
      val expected = mutable.Buffer[JobConfiguration]()

      // JSON
      val jsonJob = JobConfiguration(JobPath("/JSON") % TestVersionId, JobScript("JSON"))
      (dir / "JSON.job.json").contentString = jsonJob.asJson.toPrettyString
      expected += jsonJob.withId(JobPath("/JSON") % TestVersionId)

      // YAML
      val yamlJob = JobConfiguration(JobPath("/YAML") % TestVersionId, JobScript("YAML"))
      (dir / "YAML.job.yaml").contentString = yamlJob.asJson.toYamlString
      expected += yamlJob.withId(JobPath("/YAML") % TestVersionId)

      // XML
      val xmlJob = JobConfiguration(JobPath("/XML") % TestVersionId, JobScript("XML"))
      (dir / "XML.job.xml").xml = <job><script language="shell">XML</script></job>
      expected += xmlJob.withId(JobPath("/XML") % TestVersionId)

      assert(FileBasedReader.readDirectoryTree(JobReader :: Nil, dir, TestVersionId).map(_.toSet) ==
        Valid(expected.toSet))
    }
  }
}

object JobReaderTest {
  private val TestVersionId = VersionId("1.0.0")
}

