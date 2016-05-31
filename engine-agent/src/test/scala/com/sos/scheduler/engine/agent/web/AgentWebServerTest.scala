package com.sos.scheduler.engine.agent.web

import com.google.inject.Guice
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.inject.AgentModule
import com.sos.scheduler.engine.agent.web.AgentWebServerTest._
import com.sos.scheduler.engine.common.guice.GuiceImplicits._
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import java.net.{BindException, ServerSocket}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class AgentWebServerTest extends FreeSpec {

  "AgentStarter fails when HTTP port is not available" in {
    val port = findRandomFreeTcpPort()
    autoClosing(new ServerSocket(port)) { _ ⇒
      intercept[RuntimeException] { startAgent(port) }
        .getMessage should include (s"TCP port $port")
    }
  }

  "AgentStarter.start()" in {
    startAgent(findRandomFreeTcpPort())
  }
}

private object AgentWebServerTest {
  private def startAgent(httpPort: Int) = {
    val injector = Guice.createInjector(new AgentModule(AgentConfiguration(httpPort = Some(httpPort))))
    val agentStarter = injector.instance[AgentWebServer]
    val started = agentStarter.start()
    awaitResult(started, 10.s)
    intercept[BindException] { new ServerSocket(httpPort) }
  }
}
