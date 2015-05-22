package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.taskserver.TaskServer

/**
* @author Joacim Zschimmer
*/
final class AgentProcess(val id: AgentProcessId, val taskServer: TaskServer) extends AutoCloseable {
  def start(): Unit = taskServer.start()
  def kill(): Unit = taskServer.kill()
  def close(): Unit = taskServer.close()
}
