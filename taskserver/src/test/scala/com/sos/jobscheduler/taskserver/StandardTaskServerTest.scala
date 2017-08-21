package com.sos.jobscheduler.taskserver

//import akka.actor.ActorSystem
//import com.google.common.io.Closer
//import com.google.inject.Guice
//import com.google.inject.Stage._
//import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
//import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
//import com.sos.jobscheduler.common.scalautil.Futures._
//import com.sos.jobscheduler.common.time.ScalaTime._
//import com.sos.jobscheduler.common.utils.FreeTcpPortFinder
//import com.sos.jobscheduler.taskserver.configuration.inject.{TaskServerMainModule, TaskServerModule}
//import com.sos.jobscheduler.taskserver.data.{DotnetConfiguration, TaskServerArguments}
//import com.sos.jobscheduler.taskserver.task.RemoteModuleInstanceServer
//import java.net.{InetAddress, ServerSocket}
import org.scalatest.FreeSpec
//import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
final class StandardTaskServerTest extends FreeSpec {

  //"StandardTaskServer terminates when connection has been closed" in {
  //  val port = FreeTcpPortFinder.findRandomFreeTcpPort()
  //  val interface = "127.0.0.1"
  //  autoClosing(new ServerSocket(port, 1, InetAddress.getByName(interface))) { listener ⇒
  //    val taskServerArguments = TaskServerArguments.forTest(tcpPort = port)
  //    val injector = Guice.createInjector(PRODUCTION,
  //      new TaskServerMainModule(DotnetConfiguration()),
  //      new TaskServerModule(taskServerArguments, taskServerMainTerminated = None))
  //    autoClosing(injector.instance[Closer]) { implicit closer ⇒
  //      implicit val executionContext = injector.instance[ActorSystem].dispatcher
  //      val taskServer = new StandardTaskServer.TcpConnected {
  //        def arguments = taskServerArguments
  //        protected def newRemoteModuleInstanceServer = injector.instance[RemoteModuleInstanceServer.Factory]
  //        protected def executionContext = ExecutionContext.global
  //      }
  //      autoClosing(taskServer) { _ ⇒
  //        taskServer.start()
  //        listener.setSoTimeout(10*1000)
  //        sleep(100.ms)  // Otherwise, if it starts to fast, read() may throw an IOException "connection lost" instead of returning EOF
  //        assert(!taskServer.terminated.isCompleted)
  //        listener.accept().shutdownOutput()   // EOF
  //        awaitResult(taskServer.terminated, 1.s)
  //      }
  //    }
  //  }
  //}
}
