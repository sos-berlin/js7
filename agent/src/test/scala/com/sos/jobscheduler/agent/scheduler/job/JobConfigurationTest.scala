package com.sos.jobscheduler.agent.scheduler.job

import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.engine2.order.JobPath
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JobConfigurationTest extends FreeSpec {

  "test" in {
    val jobXml =
      <job tasks="20">
        <params>
          <param name="NAME" value="VALUE"/>
          <param name="a" value="aa"/>
        </params>
        <script language="shell">exit 0</script>
      </job>
    assert(JobConfiguration.parseXml(JobPath("/TEST-JOB"), jobXml) ==
      JobConfiguration(
        JobPath("/TEST-JOB"),
        Map("NAME" → "VALUE", "a" → "aa"),
        JobScript("exit 0"),
        taskLimit = 20))
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
    val jobPath = JobPath("/TEST")
    for (_ ← 1 to 10) {
      Stopwatch.measureTime(n, "job") {
        JobConfiguration.parseXml(jobPath, xmlString)
      }
    }
  }
}
