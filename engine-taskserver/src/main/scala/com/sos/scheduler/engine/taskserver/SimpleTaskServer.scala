package com.sos.scheduler.engine.taskserver

import akka.util.ByteString
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.common.tcp.TcpConnection
import com.sos.scheduler.engine.minicom.remoting.dialog.StandardServerDialogConnection
import com.sos.scheduler.engine.minicom.remoting.{Remoting, RemotingServer}
import com.sos.scheduler.engine.taskserver.TaskServer.Terminated
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments
import com.sos.scheduler.engine.taskserver.spoolerapi.{ProxySpooler, ProxySpoolerLog, ProxySpoolerTask}
import com.sos.scheduler.engine.taskserver.task.RemoteModuleInstanceServer
import com.sos.scheduler.engine.tunnel.data.TunnelConnectionMessage
import java.nio.channels.AsynchronousCloseException
import scala.concurrent._
import scala.util.{Failure, Success}
import spray.json._

/**
 * A blocking [[TaskServer]], running in a own thread.
 *
 * @author Joacim Zschimmer
 */
final class SimpleTaskServer(
  newRemoteModuleInstanceServer: () ⇒ RemoteModuleInstanceServer,
  val taskStartArguments: TaskStartArguments,
  isMain: Boolean = false)
  (implicit ec: ExecutionContext)
extends TaskServer with HasCloser {

  private val logger = Logger.withPrefix(getClass, taskStartArguments.agentTaskId.toString)
  private val terminatedPromise = Promise[Terminated.type]()
  private val master = TcpConnection.connect(taskStartArguments.masterInetSocketAddress).closeWithCloser

  private val remoting = new RemotingServer(
    new StandardServerDialogConnection(master),
    name = taskStartArguments.agentTaskId.toString,
    iUnknownFactories = List(
      new RemoteModuleInstanceServer.Factory {
        def newIUnknown() = newRemoteModuleInstanceServer()
      }),
    proxyIDispatchFactories = List(
      new ProxySpooler.Factory {
        val taskStartArguments = SimpleTaskServer.this.taskStartArguments
      },
      ProxySpoolerLog,
      new ProxySpoolerTask.Factory {
        val taskStartArguments = SimpleTaskServer.this.taskStartArguments
      }),
    returnAfterReleaseOf = _.isInstanceOf[RemoteModuleInstanceServer],
    keepaliveDurationOption = taskStartArguments.rpcKeepaliveDurationOption)

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
      val (correctedTried, msg) = tried match {
        case Failure(t: AsynchronousCloseException) ⇒ (Success(Terminated), s"Terminated after close(): $t")
        case Failure(t: Remoting.ConnectionClosedException) ⇒ (Success(Terminated), s"Terminated, $t")
        case _ ⇒ (tried map { _ ⇒ Terminated }, Some("Terminated") ++ tried.failed.toOption mkString ", ")
      }
      logger.info(msg)
      terminatedPromise.complete(correctedTried)
    }

  def sendProcessSignal(signal: ProcessSignal) = {
    val signalables = remoteModuleInstanceServers
    logger.debug(s"sendProcessSignal $signal to $signalables")
    signalables foreach { _.sendProcessSignal(signal) }
  }

  def deleteLogFiles() = {}  // Files are closed when master via COM RPC releases RemoteModuleInstanceServer

  override def toString = s"SimpleTaskServer(master=${taskStartArguments.masterAddress})"

  def pidOption = (remoteModuleInstanceServers flatMap { _.pidOption }).headOption

  private def remoteModuleInstanceServers = remoting.iUnknowns[RemoteModuleInstanceServer]  // Should return one
}
