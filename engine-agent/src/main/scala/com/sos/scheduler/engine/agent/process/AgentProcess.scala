package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.taskserver.TaskServer

/**
* @author Joacim Zschimmer
*/
final class AgentProcess(val id: AgentProcessId, val taskServer: TaskServer) extends AutoCloseable {
  def start(): Unit = taskServer.start()
  def sendProcessSignal(signal: ProcessSignal): Unit = taskServer.sendProcessSignal(signal)
  def close(): Unit = taskServer.close()
}
