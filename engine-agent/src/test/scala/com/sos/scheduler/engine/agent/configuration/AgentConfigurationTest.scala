package com.sos.scheduler.engine.agent.configuration

import java.nio.file.Paths
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class AgentConfigurationTest extends FreeSpec {

  "Empty argument list is rejected" in {
    intercept[NoSuchElementException] { AgentConfiguration(Nil) }
  }

  "-http-port=" in {
    assert(AgentConfiguration(List("-http-port=1234")).httpPort == 1234)
    intercept[IllegalArgumentException] { AgentConfiguration(List("-http-port=65536")) }
    intercept[IllegalArgumentException] { AgentConfiguration(httpPort = 65536) }
  }

  "-ip-address=" in {
    assert(AgentConfiguration(List("-http-port=1")).httpInterfaceRestriction.isEmpty)
    assert(AgentConfiguration(List("-http-port=1", "-ip-address=1.2.3.4")).httpInterfaceRestriction == Some("1.2.3.4"))
  }

  "-uri-prefix=" in {
    assert(AgentConfiguration(List("-http-port=1")).uriPathPrefix == "")
    assert(AgentConfiguration(List("-http-port=1", "-uri-prefix=test")).strippedUriPathPrefix == "test")
    assert(AgentConfiguration(List("-http-port=1", "-uri-prefix=/test/")).strippedUriPathPrefix == "test")
  }

  "-kill-script=" in {
    assert(AgentConfiguration(List("-http-port=1")).killScriptFile == None)
    val killScript = Paths.get("kill-script")
    assert(AgentConfiguration(List("-http-port=1", s"-kill-script=$killScript")).killScriptFile == Some(killScript.toAbsolutePath))
  }
}
