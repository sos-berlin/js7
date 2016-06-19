package com.sos.scheduler.engine.agent.configuration

import com.sos.scheduler.engine.agent.configuration.AgentConfiguration.Https
import com.sos.scheduler.engine.agent.data.ProcessKillScript
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.sprayutils.https.KeystoreReference
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.nio.file.Files.delete
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
    AgentConfiguration(Nil)
    assert(conf(Nil) == new AgentConfiguration().withConfig(AgentConfiguration.DefaultsConfig.resolve))
    assert(AgentConfiguration() == new AgentConfiguration)
  }

  "-https-port=" in {
    assert((conf(Nil).https map { _.port }) == None)
    intercept[IllegalArgumentException] { conf(List("-https-port=1234")).https }
    intercept[IllegalArgumentException] { conf(List("-https-port=1234")).https }
    intercept[IllegalArgumentException] { conf(List("-data-directory=/TEST/DATA", "-https-port=65536")) }
    assert(conf(List("-data-directory=/TEST/DATA", "-https-port=1234")).https contains Https(
      1234,
      KeystoreReference(
        url = (Paths.get("/TEST/DATA").toAbsolutePath / "config/private/private-https.jks").toUri.toURL,
        storePassword = Some(SecretString("jobscheduler")),
        keyPassword = Some(SecretString("jobscheduler")))))
  }

  "-http-port=" in {
    assert(conf(List("-http-port=1234")).httpPort contains 1234)
    intercept[IllegalArgumentException] { conf(List("-http-port=65536")) }
    intercept[IllegalArgumentException] { AgentConfiguration(httpPort = Some(65536)) }
  }

  "-ip-address=" in {
    assert(conf(Nil).httpInterfaceRestriction == Some("0.0.0.0"))
    assert(conf(List("-ip-address=1.2.3.4")).httpInterfaceRestriction == Some("1.2.3.4"))
  }

  "-log-directory=" in {
    assert(conf(Nil).logDirectory == temporaryDirectory)
    assert(conf(List("-data-directory=TEST/DATA")).logDirectory == Paths.get("TEST/DATA/logs").toAbsolutePath)
    assert(conf(List("-data-directory=TEST/DATA", "-log-directory=LOGS")).logDirectory == Paths.get("LOGS").toAbsolutePath)
    assert(conf(List("-log-directory=test")).logDirectory == Paths.get("test").toAbsolutePath)
  }

  "-uri-prefix=" in {
    assert(conf(Nil).uriPathPrefix == "")
    assert(conf(List("-uri-prefix=test")).strippedUriPathPrefix == "test")
    assert(conf(List("-uri-prefix=/test/")).strippedUriPathPrefix == "test")
  }

  "-kill-script=" in {
    val generatedFile = conf(Nil).logDirectory / s"jobscheduler_agent_11111_kill_task.${if (isWindows) "cmd" else "sh"}"
    assert(AgentConfiguration(List("-http-port=11111")).finishAndProvideFiles.killScript == Some(ProcessKillScript(generatedFile)))
    delete(generatedFile)
    assert(conf(List("-data-directory=TEST/DATA", "-kill-script=")).finishAndProvideFiles.killScript == None)

    val killScript = Paths.get("kill-script")
    assert(conf(List("-data-directory=TEST/DATA", s"-kill-script=$killScript")).killScript == Some(ProcessKillScript(killScript.toAbsolutePath)))
  }

  "-rpc-keepalive=" in {
    assert(conf(Nil).rpcKeepaliveDuration == None)
    assert(conf(List("-rpc-keepalive=5m")).rpcKeepaliveDuration == Some(5 * 60.s))
  }

  private def conf(args: List[String]) = AgentConfiguration(args)
}
