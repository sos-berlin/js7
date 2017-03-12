package com.sos.jobscheduler.taskserver

import akka.util.ByteString
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger}
import com.sos.jobscheduler.common.tcp.BlockingTcpConnection
import com.sos.jobscheduler.minicom.remoting.dialog.{ServerDialogConnection, StandardServerDialogConnection}
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
trait StandardTaskServer
extends TaskServer with HasCloser {

  protected def serverDialogConnection: ServerDialogConnection
  protected def newRemoteModuleInstanceServer: TaskServerArguments ⇒ RemoteModuleInstanceServer
  protected def isMain: Boolean = false
  protected implicit def executionContext: ExecutionContext
  protected def beforeStart() = {}
  protected def afterStop() = {}

  private val logger = Logger.withPrefix[StandardTaskServer](arguments.agentTaskId.toString)
  private val terminatedPromise = Promise[Terminated.type]()

  private lazy val remoting = new ServerRemoting(
    serverDialogConnection,
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

  final def terminated = terminatedPromise.future

  final def start(): Unit = {
    (for (_ ← { blocking { beforeStart() }; remoting.run() };
          _ ← Future { blocking { afterStop() }}) yield ()
    ) onComplete { tried ⇒
      val (correctedTried, msg) = tried match {
        case Failure(t: AsynchronousCloseException)         ⇒ (Success(Terminated), s"Terminated after close(): $t")
        case Failure(t: Remoting.ConnectionClosedException) ⇒ (Success(Terminated), s"Terminated, $t")
        case _ ⇒ (tried map { _ ⇒ Terminated }, Some("Terminated") ++ tried.failed.toOption mkString ", ")
      }
      correctedTried match {
        case Failure(t) ⇒ logger.error(s"Task terminated with error: $t", t)
        case Success(Terminated) ⇒ logger.info(msg)
      }
      terminatedPromise.complete(correctedTried)
    }
  }

  final def sendProcessSignal(signal: ProcessSignal) = {
    val signalables = remoteModuleInstanceServers
    logger.debug(s"sendProcessSignal $signal to $signalables")
    signalables foreach { _.sendProcessSignal(signal) }
  }

  final def deleteLogFiles() = {}  // Files are closed when master via COM RPC releases RemoteModuleInstanceServer

  override def toString = s"StandardTaskServer(master=${arguments.masterAddress})"

  final def pidOption = (remoteModuleInstanceServers flatMap { _.pidOption }).headOption

  private def remoteModuleInstanceServers = remoting.iUnknowns[RemoteModuleInstanceServer]  // Should return one
}

object StandardTaskServer {
  trait TcpConnected extends StandardTaskServer {
    this: StandardTaskServer ⇒

    private val connection = BlockingTcpConnection.connect(arguments.masterInetSocketAddress)
    protected final lazy val serverDialogConnection = new StandardServerDialogConnection(connection)(executionContext)

    override protected def beforeStart(): Unit = {
      super.beforeStart()
      connection.sendMessage(ByteString(TunnelConnectionMessage(arguments.tunnelToken).toJson.compactPrint))
    }

    override protected def afterStop(): Unit = {
      connection.close()
      super.afterStop()
    }
  }
}
