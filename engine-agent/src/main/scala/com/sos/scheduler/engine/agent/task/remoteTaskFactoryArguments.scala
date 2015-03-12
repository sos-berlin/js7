package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.taskserver.task.TaskStartArguments

/**
 * @author Joacim Zschimmer
 */
sealed trait RemoteTaskFactoryArguments {
  def startArguments: TaskStartArguments
}

final case class InProcessRemoteTaskFactoryArguments(startArguments: TaskStartArguments)
extends RemoteTaskFactoryArguments

final case class DedicatedProcessRemoteTaskFactoryArguments(javaOptions: String, javaClasspath: String, startArguments: TaskStartArguments)
extends RemoteTaskFactoryArguments
