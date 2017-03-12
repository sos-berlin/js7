package com.sos.jobscheduler.taskserver

import com.google.common.io.{ByteStreams, Closer}
import com.google.inject.Guice
import com.google.inject.Stage._
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.taskserver.StandardTaskServer.TcpConnected
import com.sos.jobscheduler.taskserver.configuration.inject.{TaskServerMainModule, TaskServerModule}
import com.sos.jobscheduler.taskserver.data.{TaskServerArguments, TaskServerMainTerminated}
import com.sos.jobscheduler.taskserver.task.RemoteModuleInstanceServer
import scala.concurrent.{ExecutionContext, Promise}
import spray.json._

/**
 * @author Joacim Zschimmer
 */
object TaskServerMain {

  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit = {
    CommandLineArguments.parse(args) { _.optionAs[String]("-agent-task-id=") }  // -agent-task-id=.. is only for the kill script and ignored
    try {
      val startArguments = new JsonParser(ByteStreams.toByteArray(System.in)).parseJsValue().asJsObject.convertTo[TaskServerArguments]
      run(startArguments)
      logger.info("Terminating")
    } catch {
      case t: Throwable ⇒
        logger.error(s"$t", t)
        System.err.println(t.toString)
        System.exit(1)
    }
  }

  private def run(startArguments: TaskServerArguments): Unit = {
    val terminated = Promise[TaskServerMainTerminated.type]()
    val injector = Guice.createInjector(PRODUCTION,
      new TaskServerMainModule(startArguments.dotnet),
      new TaskServerModule(startArguments, Some(terminated.future))
    )
    autoClosing(injector.instance[Closer]) { implicit closer ⇒
      implicit val ec = injector.instance[ExecutionContext]
      val taskServer = new TcpConnected {
        def arguments = startArguments
        protected def newRemoteModuleInstanceServer = injector.instance[RemoteModuleInstanceServer.Factory]
        protected def executionContext = ec
        override def isMain = true
      }
      autoClosing(taskServer) { _ ⇒
        taskServer.terminated map { _: TaskServer.Terminated.type ⇒ TaskServerMainTerminated } onComplete terminated.complete
        taskServer.start()
        taskServer.terminated.awaitInfinite
      }
    }
  }
}
