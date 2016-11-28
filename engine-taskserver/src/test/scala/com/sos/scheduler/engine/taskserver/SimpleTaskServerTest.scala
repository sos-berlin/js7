package com.sos.scheduler.engine.taskserver

import akka.actor.ActorSystem
import com.google.common.io.Closer
import com.google.inject.Guice
import com.google.inject.Stage._
import com.sos.scheduler.engine.common.guice.GuiceImplicits.RichInjector
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder
import com.sos.scheduler.engine.taskserver.configuration.inject.{TaskServerMainModule, TaskServerModule}
import com.sos.scheduler.engine.taskserver.data.{DotnetConfiguration, TaskStartArguments}
import com.sos.scheduler.engine.taskserver.task.RemoteModuleInstanceServer
import java.net.{InetAddress, ServerSocket}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class SimpleTaskServerTest extends FreeSpec {

  "SimpleTaskServer terminates when connection has been closed" in {
    val port = FreeTcpPortFinder.findRandomFreeTcpPort()
    val interface = "127.0.0.1"
    autoClosing(new ServerSocket(port, 1, InetAddress.getByName(interface))) { listener ⇒
      val taskStartArguments = TaskStartArguments.forTest(tcpPort = port)
      val injector = Guice.createInjector(PRODUCTION,
        new TaskServerMainModule(DotnetConfiguration()),
        new TaskServerModule(taskStartArguments, taskServerMainTerminated = None))
      implicit val executionContext = injector.instance[ActorSystem].dispatcher
      autoClosing(injector.instance[Closer]) { _ ⇒
        autoClosing(new SimpleTaskServer(() ⇒ injector.instance[RemoteModuleInstanceServer], taskStartArguments)) { server ⇒
          server.start()
          listener.setSoTimeout(10*1000)
          sleep(100.ms)  // Otherwise, if it starts to fast, read() may throw an IOException "connection lost" instead of returning EOF
          assert(!server.terminated.isCompleted)
          listener.accept().shutdownOutput()   // EOF
          awaitResult(server.terminated, 1.s)
        }
      }
    }
  }
}
