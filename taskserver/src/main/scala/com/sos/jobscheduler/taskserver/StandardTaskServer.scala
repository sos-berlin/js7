package com.sos.jobscheduler.taskserver

import akka.util.ByteString
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.common.scalautil.{HasCloser, Logger}
import com.sos.jobscheduler.minicom.remoting.dialog.ServerDialogConnection
import com.sos.jobscheduler.minicom.remoting.{Remoting, ServerRemoting}
import com.sos.jobscheduler.taskserver.data.TaskServerArguments
import com.sos.jobscheduler.taskserver.spoolerapi.{ProxySpooler, ProxySpoolerLog, ProxySpoolerTask}
import com.sos.jobscheduler.taskserver.task.RemoteModuleInstanceServer
import java.nio.channels.AsynchronousCloseException
import scala.concurrent._
import scala.util.{Failure, Success}

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
  private val terminatedPromise = Promise[Completed]()

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
          _ ← Future { blocking { afterStop() }}) yield Completed
    ) onComplete { tried ⇒
      val (correctedTried, msg) = tried match {
        case Failure(t: AsynchronousCloseException)         ⇒ (Success(Completed), s"Terminated after close(): $t")
        case Failure(t: Remoting.ConnectionClosedException) ⇒ (Success(Completed), s"Terminated, $t")
        case _ ⇒ (tried, Some("Terminated") ++ tried.failed.toOption mkString ", ")
      }
      correctedTried match {
        case Failure(t) ⇒ logger.error(s"Task terminated with error: $t", t)
        case Success(Completed) ⇒ logger.info(msg)
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

  override def toString = "StandardTaskServer"

  final def pidOption = (remoteModuleInstanceServers flatMap { _.pidOption }).headOption

  private def remoteModuleInstanceServers = remoting.iUnknowns[RemoteModuleInstanceServer]  // Should return one
}

object StandardTaskServer {

  trait LocalConnection {
    def request: ByteString ⇒ Future[ByteString]
  }
}
