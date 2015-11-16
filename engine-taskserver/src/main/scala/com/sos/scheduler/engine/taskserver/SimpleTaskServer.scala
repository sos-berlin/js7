package com.sos.scheduler.engine.taskserver

import akka.util.ByteString
import com.google.inject.Guice
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.common.tcp.TcpConnection
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.minicom.remoting.{DialogConnection, Remoting}
import com.sos.scheduler.engine.taskserver.SimpleTaskServer._
import com.sos.scheduler.engine.taskserver.configuration.inject.TaskServerModule
import com.sos.scheduler.engine.taskserver.data.{HasSendProcessSignal, TaskStartArguments}
import com.sos.scheduler.engine.taskserver.spoolerapi.{ProxySpooler, ProxySpoolerLog, ProxySpoolerTask}
import com.sos.scheduler.engine.taskserver.task.RemoteModuleInstanceServer
import com.sos.scheduler.engine.tunnel.data.TunnelConnectionMessage
import java.nio.channels.AsynchronousCloseException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import spray.json._

/**
 * A blocking [[TaskServer]], running in a own thread.
 *
 * @author Joacim Zschimmer
 */
final class SimpleTaskServer(val taskStartArguments: TaskStartArguments, isMain: Boolean = false) extends TaskServer with HasCloser {

  private lazy val master = TcpConnection.connect(taskStartArguments.masterInetSocketAddress).closeWithCloser
  private val terminatedPromise = Promise[Unit]()
  private val injector = Guice.createInjector(new TaskServerModule(taskStartArguments, taskServerMainTerminated = isMain option terminated))
  private val remoting = new Remoting(injector, new DialogConnection(master), IDispatchFactories, ProxyIDispatchFactories)

  def terminated = terminatedPromise.future

  def start(): Unit =
    Future {
      blocking {
        val connectionMessage = TunnelConnectionMessage(taskStartArguments.tunnelToken)
        master.sendMessage(ByteString(connectionMessage.toJson.compactPrint))
        remoting.run()
        master.close()
      }
    } onComplete { tried ⇒
      terminatedPromise.complete(tried recover {
        case t: AsynchronousCloseException ⇒
          logger.info("Terminating after close()")
          // Exception is okay and ignored
        case t ⇒
          logger.error(s"$toString $t", t)
          throw t
      })
      logger.info("Terminated")
    }

  def sendProcessSignal(signal: ProcessSignal) = {
    val signalables = remoting.invocables[HasSendProcessSignal]
    logger.debug(s"sendProcessSignal $signal to $signalables")
    signalables foreach { _.sendProcessSignal(signal) }
  }

  override def toString = s"TaskServer(master=${taskStartArguments.masterAddress})"

  def pidOption = (remoting.invocables[RemoteModuleInstanceServer] flatMap { _.pidOption }).headOption
}

object SimpleTaskServer {
  private val IDispatchFactories = List(RemoteModuleInstanceServer)
  private val ProxyIDispatchFactories = List(ProxySpooler, ProxySpoolerLog, ProxySpoolerTask)
  private val logger = Logger(getClass)

  def runAsMain(startArguments: TaskStartArguments): Unit =
    autoClosing(new SimpleTaskServer(startArguments, isMain = true)(global)) { taskServer ⇒
      taskServer.start()
      awaitResult(taskServer.terminated, MaxDuration)
    }
}
