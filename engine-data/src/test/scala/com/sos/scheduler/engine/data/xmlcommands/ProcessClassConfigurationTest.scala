package com.sos.scheduler.engine.data.xmlcommands

import com.sos.scheduler.engine.data.agent.AgentAddress
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ProcessClassConfigurationTest extends FreeSpec {

  "Empty" in {
    assert(ProcessClassConfiguration().xmlElem == <process_class/>)
  }

  "processMaximum" in {
    assert(ProcessClassConfiguration(processMaximum = Some(123)).xmlElem == <process_class max_processes="123"/>)
  }

  "With one agent" in {
    assert(ProcessClassConfiguration(agentUris = List(AgentAddress("one"))).xmlElem == <process_class remote_scheduler="one"/>)
  }

  "With multiple agent" in {
    assert(ProcessClassConfiguration(agentUris = List(AgentAddress("one"), AgentAddress("two"))).xmlElem ==
      <process_class><remote_schedulers><remote_scheduler remote_scheduler="one"/><remote_scheduler remote_scheduler="two"/></remote_schedulers></process_class>)
  }
}
