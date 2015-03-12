package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.common.scalautil.Logger

/**
 * @author Joacim Zschimmer
 */
final class DedicatedProcessRemoteTask(val launchArguments: RemoteTaskFactoryArguments) extends RemoteTask {
  def id = launchArguments.startArguments.remoteTaskId

  def start() = ???

  def close() = ???

  def kill() = ???
}

object DedicatedProcessRemoteTask {
  private val logger = Logger(getClass)
}
