package js7.tests.controller.proxy.servlet

import cats.effect.{IO, Resource, ResourceIO}
import jakarta.servlet.Servlet
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.DelayConf
import org.eclipse.jetty.ee10.servlet.{DefaultServlet, ServletContextHandler, ServletHolder}
import org.eclipse.jetty.server.{Server, ServerConnector}

object JettyWebServer:

  def resource(interface: String, port: Int, servlet: Iterable[(String, Servlet)]): ResourceIO[Server] =
    Resource.make(
      acquire = IO.blocking:
        val server = new Server

        server.addConnector:
          val connector = new ServerConnector(server)
          connector.setHost(interface)
          connector.setPort(port)
          connector

        server.setHandler:
          val handler = new ServletContextHandler(ServletContextHandler.NO_SESSIONS)
          servlet.foreach: (path, servlet) =>
            handler.addServlet(new ServletHolder(servlet), path)
          handler.addServlet(classOf[DefaultServlet], "/") // 404 for everything else
          handler
        server.start()
        server)(
      release = server =>
        IO.blocking:
          server.stop()
          server.join())
      .evalTap: server =>
        DelayConf(10.ms, 30.ms, 60.ms, 100.ms).tailRecM[IO, Unit]: delayer =>
          if server.isStarting then
            delayer.sleep.as(Left(()))
          else
            IO.right(())
        .logWhenItTakesLonger("Jetty start")
