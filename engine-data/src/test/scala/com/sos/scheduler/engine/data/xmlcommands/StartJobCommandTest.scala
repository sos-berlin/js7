package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.job.JobPath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class StartJobCommandTest extends FreeSpec {

  "StartJobCommand" in {
    assert(StartJobCommand(JobPath("/test")).xmlElem == <start_job job="/test"><params/></start_job>)
    assert(StartJobCommand(JobPath("/test"), variables = List("a" → "1")).xmlElem ==
      <start_job job="/test"><params><param name="a" value="1"/></params></start_job>)
  }
}
