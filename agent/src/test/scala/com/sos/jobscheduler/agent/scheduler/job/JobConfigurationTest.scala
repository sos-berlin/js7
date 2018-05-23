package com.sos.jobscheduler.agent.scheduler.job

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.tester.CirceJsonTester
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JobConfigurationTest extends FreeSpec {

  "JSON, anonymous" in {
    CirceJsonTester.testJson(
      JobConfiguration(
        JobPath.NoId,
        JobScript("echo HELLO\n"),
        Map("VARIABLE" → "VALUE"),
        taskLimit = 3),
      json"""{
        "script": "echo HELLO\n",
        "variables": {
          "VARIABLE": "VALUE"
        },
        "taskLimit": 3
      }""")
  }

  "JSON, with Id" in {
    CirceJsonTester.testJson(
      JobConfiguration(
        JobPath("/JOB") % "1",
        JobScript("echo HELLO\n"),
        Map("VARIABLE" → "VALUE"),
        taskLimit = 3),
      json"""{
        "id": {
          "path": "/JOB",
          "versionId": "1"
        },
        "script": "echo HELLO\n",
        "variables": {
          "VARIABLE": "VALUE"
        },
        "taskLimit": 3
      }""")
  }

  "test" in {
    val jobXml =
      <job tasks="20">
        <params>
          <param name="NAME" value="VALUE"/>
          <param name="a" value="aa"/>
        </params>
        <script language="shell">exit 0</script>
      </job>
    assert(JobConfiguration.parseXml(JobPath("/TEST-JOB") % "VERSION", jobXml) ==
      Valid(JobConfiguration(
        JobPath("/TEST-JOB") % "VERSION",
        JobScript("exit 0"),
        Map("NAME" → "VALUE", "a" → "aa"),
        taskLimit = 20)))
  }

  if (sys.props contains "test.speed") "Speed" in {
    val n = 10000
    val xmlString =
      <job>
        <params>{
          for (i ← 1 to 10) yield <param name={s"NAME-$i"} value={"*" * 100}/>
        }</params>
        <script language="shell">SCRIPT</script>
      </job>.toString
    val jobId = JobPath("/TEST") % "VERSION"
    for (_ ← 1 to 10) {
      info(Stopwatch.measureTime(n, "job") {
        JobConfiguration.parseXml(jobId, xmlString)
      }.toString)
    }
  }
}
