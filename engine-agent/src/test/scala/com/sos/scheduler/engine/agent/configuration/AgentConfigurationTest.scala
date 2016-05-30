package com.sos.scheduler.engine.agent.configuration

import com.sos.scheduler.engine.agent.configuration.AgentConfiguration.{Https, UseInternalKillScript}
import com.sos.scheduler.engine.agent.data.ProcessKillScript
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.sprayutils.https.KeystoreReference
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

  "Empty argument list" in {
    assert(AgentConfiguration(Nil) == AgentConfiguration())
  }

  "-https-port=" in {
    assert(AgentConfiguration(List("-https-port=1234")).https map { _.port }  contains 1234)
    intercept[IllegalArgumentException] { AgentConfiguration(List("-https-port=65536")) }
    intercept[IllegalArgumentException] { AgentConfiguration() withHttpsPort 65536 }
    assert(AgentConfiguration(List("-data-directory=/TEST/DATA", "-https-port=1234")).https contains Https(
      1234,
      KeystoreReference(
        url = (Paths.get("/TEST/DATA").toAbsolutePath / "config/private/https.jks").toUri.toURL,
        storePassword = Some(SecretString("jobscheduler")),
        keyPassword = SecretString("jobscheduler"))))
  }

  "-http-port=" in {
    assert(AgentConfiguration(List("-http-port=1234")).httpPort contains 1234)
    intercept[IllegalArgumentException] { AgentConfiguration(List("-http-port=65536")) }
    intercept[IllegalArgumentException] { AgentConfiguration(httpPort = Some(65536)) }
  }

  "-http-port= and -https-port= cannot be combined" in {
    intercept[IllegalArgumentException] { AgentConfiguration(List("-http-port=11111", "-https-port=22222")) }
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
    assert(conf(Nil).rpcKeepaliveDuration == None)
    assert(conf(List("-rpc-keepalive=5m")).rpcKeepaliveDuration == Some(5 * 60.s))
  }

  private def conf(args: Seq[String]) = AgentConfiguration(args)
}
