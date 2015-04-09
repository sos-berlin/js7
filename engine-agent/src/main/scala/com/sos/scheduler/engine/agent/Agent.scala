package com.sos.scheduler.engine.agent

import com.google.common.io.Closer
import com.google.inject.Guice
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.inject.AgentModule
import com.sos.scheduler.engine.agent.web.AgentWebServer
import com.sos.scheduler.engine.common.guice.GuiceImplicits._
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class Agent(configuration: AgentConfiguration) extends AutoCloseable {

  private val injector = Guice.createInjector(new AgentModule(configuration))
  private val server = injector.instance[AgentWebServer]
  private val closer = injector.instance[Closer]

  def start(): Future[Unit] = server.start()

  def close(): Unit = closer.close()
}

object Agent {
  def main(args: Array[String]): Unit = run(AgentConfiguration(args))

  def run(conf: AgentConfiguration): Unit =
    autoClosing(new Agent(conf)) { agent ⇒
      agent.start()
      Thread.sleep(Int.MaxValue)  // ??? Warten, bis Agent per Kommando beendet wird
    }
}
