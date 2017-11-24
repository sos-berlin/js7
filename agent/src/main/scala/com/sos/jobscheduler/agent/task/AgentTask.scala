package com.sos.jobscheduler.agent.task

import akka.util.ByteString
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.views.TaskOverview
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.utils.HasKey
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.common.scalautil.Closers._
import com.sos.jobscheduler.data.workflow.JobPath
import com.sos.jobscheduler.taskserver.TaskServer
import java.time.Instant.now
import scala.concurrent.Future

/**
* @author Joacim Zschimmer
*/
trait AgentTask
extends AutoCloseable
with BaseAgentTask
with HasKey {

  final type Key = AgentTaskId
  final def key = id

  def id: AgentTaskId

  def jobPath: JobPath

  protected def apiConnection: ApiConnection

  protected def taskServer: TaskServer

  private val startedAt = now

  final def closeApiConnectionAndTaskServer() = closeOrdered(apiConnection, taskServer)  // Close connection before taskServer

  final def start(): Unit = taskServer.start()

  final def request(byteString: ByteString): Future[ByteString] =
    apiConnection.request(byteString)

  final def sendProcessSignal(signal: ProcessSignal): Unit = taskServer.sendProcessSignal(signal)

  final def deleteLogFiles(): Unit = taskServer.deleteLogFiles()

  final def terminated = taskServer.terminated

  final def pidOption: Option[Pid] = taskServer.pidOption

  final def overview = TaskOverview(jobPath, id, pid = taskServer.pidOption, startedAt)

  override def toString = s"$id: $taskServer"
}
