package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.taskserver.SimpleTaskServer
import com.sos.scheduler.engine.taskserver.task.StartConfiguration

/**
 * @author Joacim Zschimmer
 */
final class InProcessRemoteTask(val configuration: StartConfiguration) extends RemoteTask {

  def id = configuration.remoteTaskId

  private val taskServer = new SimpleTaskServer(configuration)
  
  def start(): Unit = taskServer.start()

  def kill(): Unit = taskServer.kill()

  def close(): Unit = taskServer.close()

  override def toString = taskServer.toString
}
