package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import com.google.inject.Guice
import com.sos.scheduler.engine.agent.commands.Command
import com.sos.scheduler.engine.agent.web.AgentWebServerTest._
import com.sos.scheduler.engine.agent.{AgentConfiguration, CommandExecutor}
import com.sos.scheduler.engine.common.guice.GuiceImplicits._
import com.sos.scheduler.engine.common.guice.ScalaAbstractModule
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.scheduler.engine.test.scalatest.HasCloserBeforeAndAfterAll
import java.net.{BindException, ServerSocket}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class AgentWebServerTest extends FreeSpec with HasCloserBeforeAndAfterAll {

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
  private object TestCommandExecutor extends CommandExecutor {
    def executeCommand(command: Command) = throw new NotImplementedError
  }

  private def startAgent(httpPort: Int) = {
    val injector = Guice.createInjector(new ScalaAbstractModule {
      def configure() = {
        bindInstance[AgentConfiguration](AgentConfiguration(httpPort = httpPort))
        bindInstance[CommandExecutor](TestCommandExecutor)
        bindInstance[ActorSystem](ActorSystem("TEST"))
      }
    })
    val agentStarter = injector.apply[AgentWebServer]
    val started = agentStarter.start()
    Await.result(started, 10.seconds)
    intercept[BindException] { new ServerSocket(httpPort) }
  }
}
