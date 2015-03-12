package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.taskserver.SimpleTaskServer
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments

/**
 * @author Joacim Zschimmer
 */
final class InProcessRemoteTask(val startArguments: TaskStartArguments) extends RemoteTask {

  def id = startArguments.remoteTaskId

  private val taskServer = new SimpleTaskServer(startArguments)
  
  def start(): Unit = taskServer.start()

  def kill(): Unit = taskServer.kill()

  def close(): Unit = taskServer.close()

  override def toString = taskServer.toString
}
