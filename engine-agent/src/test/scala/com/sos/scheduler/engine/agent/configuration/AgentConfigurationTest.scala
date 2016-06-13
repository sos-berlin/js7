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
    intercept[NoSuchElementException] { AgentConfiguration(Nil) }
    assert(AgentConfiguration.Default == AgentConfiguration())
  }

  "-https-port=" in {
    assert(conf(List("-https-port=1234")).https map { _.port }  contains 1234)
    intercept[IllegalArgumentException] { conf(List("-https-port=65536")) }
    intercept[IllegalArgumentException] { conf(Nil) withHttpsPort 65536 }
    assert(conf(List("-https-port=1234")).https contains Https(
      1234,
      KeystoreReference(
        url = (Paths.get("/TEST/DATA").toAbsolutePath / "config/private/https.jks").toUri.toURL,
        storePassword = Some(SecretString("jobscheduler")),
        keyPassword = SecretString("jobscheduler"))))
  }

  "-http-port=" in {
    assert(conf(List("-http-port=1234")).httpPort contains 1234)
    intercept[IllegalArgumentException] { conf(List("-http-port=65536")) }
    intercept[IllegalArgumentException] { AgentConfiguration(httpPort = Some(65536)) }
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

  private def conf(args: List[String]) = AgentConfiguration("-data-directory=/TEST/DATA" :: args)
}
