package com.sos.scheduler.engine.agent.main

import com.google.common.io.Closer
import com.google.inject.Guice
import com.sos.scheduler.engine.agent.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.inject.AgentModule
import com.sos.scheduler.engine.agent.web.AgentWebServer
import com.sos.scheduler.engine.common.guice.GuiceImplicits._
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing

/**
 * @author Joacim Zschimmer
 */
final class Main(conf: AgentConfiguration) extends AutoCloseable {

  private val injector = Guice.createInjector(new AgentModule(conf))
  private val closer = injector.apply[Closer]
  private val server = injector.apply[AgentWebServer]

  def start() = server.start()

  def close() = closer.close()
}

object Main {
  def main(args: Array[String]): Unit = run(AgentConfiguration(args))

  def run(conf: AgentConfiguration): Unit =
    autoClosing(new Main(conf)) { m ⇒
      m.start()
      Thread.sleep(Int.MaxValue)  // ??? Warten, bis Agent per Kommando beendet wird
    }
}
