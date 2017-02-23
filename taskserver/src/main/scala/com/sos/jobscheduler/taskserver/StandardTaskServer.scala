package com.sos.jobscheduler.taskserver

import akka.util.ByteString
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Futures.namedThreadFuture
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger}
import com.sos.jobscheduler.common.tcp.BlockingTcpConnection
import com.sos.jobscheduler.common.time.ScalaTime.MaxDuration
import com.sos.jobscheduler.minicom.remoting.dialog.StandardServerDialogConnection
import com.sos.jobscheduler.minicom.remoting.{Remoting, ServerRemoting}
import com.sos.jobscheduler.taskserver.TaskServer.Terminated
import com.sos.jobscheduler.taskserver.data.TaskServerArguments
import com.sos.jobscheduler.taskserver.spoolerapi.{ProxySpooler, ProxySpoolerLog, ProxySpoolerTask}
import com.sos.jobscheduler.taskserver.task.RemoteModuleInstanceServer
import com.sos.jobscheduler.tunnel.data.TunnelConnectionMessage
import java.nio.channels.AsynchronousCloseException
import scala.concurrent._
import scala.util.{Failure, Success}
import spray.json._

/**
 * A blocking [[TaskServer]], running in a own thread.
 *
 * @author Joacim Zschimmer
 */
final class StandardTaskServer(
  newRemoteModuleInstanceServer: TaskServerArguments ⇒ RemoteModuleInstanceServer,
  val arguments: TaskServerArguments,
  isMain: Boolean = false)
  (implicit ec: ExecutionContext)
extends TaskServer with HasCloser {

  private val logger = Logger.withPrefix[StandardTaskServer](arguments.agentTaskId.toString)
  private val terminatedPromise = Promise[Terminated.type]()
  private val master = BlockingTcpConnection.connect(arguments.masterInetSocketAddress).closeWithCloser

  private val remoting = new ServerRemoting(
    new StandardServerDialogConnection(master),
    name = arguments.agentTaskId.toString,
    iUnknownFactories = List(
      new RemoteModuleInstanceServer.MyIUnknownFactory {
        def newIUnknown() = newRemoteModuleInstanceServer(arguments)
      }),
    proxyIDispatchFactories = List(
      new ProxySpooler.Factory {
        val taskServerArguments = arguments
      },
      ProxySpoolerLog,
      new ProxySpoolerTask.Factory {
        val taskServerArguments = arguments
      }),
    returnAfterReleaseOf = _.isInstanceOf[RemoteModuleInstanceServer],
    keepaliveDurationOption = arguments.rpcKeepaliveDurationOption)

  def terminated = terminatedPromise.future

  def start(): Unit =
    namedThreadFuture("Job " + arguments.startMeta.job) {
      val connectionMessage = TunnelConnectionMessage(arguments.tunnelToken)
      master.sendMessage(ByteString(connectionMessage.toJson.compactPrint))
      remoting.run() await MaxDuration
      master.close()
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

  override def toString = s"StandardTaskServer(master=${arguments.masterAddress})"

  def pidOption = (remoteModuleInstanceServers flatMap { _.pidOption }).headOption

  private def remoteModuleInstanceServers = remoting.iUnknowns[RemoteModuleInstanceServer]  // Should return one
}
