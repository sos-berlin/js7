package com.sos.scheduler.engine.agent.process

import com.sos.scheduler.engine.taskserver.SimpleTaskServer

/**
 * Not a real operating system process, runs in our JVM.
 *
 * @author Joacim Zschimmer
 */
final class DummyAgentProcess(val arguments: AgentProcessArguments) extends AgentProcess {

  def id = arguments.processId

  private val taskServer = new SimpleTaskServer(arguments.taskStartArguments)
  
  def start(): Unit = taskServer.start()

  def kill(): Unit = taskServer.kill()

  def close(): Unit = taskServer.close()

  override def toString = taskServer.toString
}
