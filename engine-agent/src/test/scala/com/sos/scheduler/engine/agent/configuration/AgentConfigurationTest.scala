package com.sos.scheduler.engine.agent.configuration

import com.sos.scheduler.engine.agent.configuration.AgentConfiguration.UseInternalKillScript
import com.sos.scheduler.engine.agent.data.ProcessKillScript
import com.sos.scheduler.engine.common.system.FileUtils.temporaryDirectory
import com.sos.scheduler.engine.common.time.ScalaTime._
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
    assert(conf(Nil).httpInterfaceRestriction.isEmpty)
    assert(conf(List("-ip-address=1.2.3.4")).httpInterfaceRestriction == Some("1.2.3.4"))
  }

  "-log-directory=" in {
    assert(conf(Nil).uriPathPrefix == "")
    assert(conf(Nil).logDirectory == temporaryDirectory)
    assert(conf(List("-log-directory=test")).logDirectory == Paths.get("test").toAbsolutePath)
  }

  "-uri-prefix=" in {
    assert(conf(Nil).uriPathPrefix == "")
    assert(conf(List("-uri-prefix=test")).strippedUriPathPrefix == "test")
    assert(conf(List("-uri-prefix=/test/")).strippedUriPathPrefix == "test")
  }

  "-kill-script=" in {
    assert(conf(Nil).killScript == Some(UseInternalKillScript))
    assert(conf(List(s"-kill-script=")).killScript == None)
    val killScript = Paths.get("kill-script")
    assert(conf(List(s"-kill-script=$killScript")).killScript == Some(ProcessKillScript(killScript.toAbsolutePath)))
  }

  "-rpc-keepalive=" in {
    assert(conf(Nil).rpcKeepaliveDuration == Some(5 * 60.s))
    assert(conf(List("-rpc-keepalive=5m")).rpcKeepaliveDuration == Some(5 * 60.s))
  }

  private def conf(args: Seq[String]) = AgentConfiguration(List("-http-port=1") ++ args)
}
