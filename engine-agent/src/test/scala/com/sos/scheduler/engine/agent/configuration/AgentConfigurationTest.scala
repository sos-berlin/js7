package com.sos.scheduler.engine.agent.configuration

import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.system.FileUtils.temporaryDirectory
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
    assert(conf(Nil).killScriptFile == None)
    val killScript = Paths.get("kill-script")
    assert(conf(List(s"-kill-script=$killScript")).killScriptFile == Some(killScript.toAbsolutePath))
  }

  "-rpc-keepalive=" in {
    assert(conf(Nil).rpcKeepaliveDuration == None)
    assert(conf(List("-rpc-keepalive=300")).rpcKeepaliveDuration == Some(300.s))
  }

  "-kill-after-tunnel-timeout=" in {
    assert(conf(Nil).killAfterTunnelTimeout == None)
    assert(conf(List("-kill-after-tunnel-timeout=10", "-rpc-keepalive=9")).killAfterTunnelTimeout == Some(10.s))
    intercept[IllegalArgumentException] {
      conf(List("-kill-after-tunnel-timeout=10", "-rpc-keepalive=10"))
    }
    intercept[IllegalArgumentException] {
      conf(List("-kill-after-tunnel-timeout=10", "-rpc-keepalive=11"))
    }
    intercept[IllegalArgumentException] {
      conf(List("-kill-after-tunnel-timeout=10"))
    }
  }

  private def conf(args: Seq[String]) = AgentConfiguration(List("-http-port=1") ++ args)
}
