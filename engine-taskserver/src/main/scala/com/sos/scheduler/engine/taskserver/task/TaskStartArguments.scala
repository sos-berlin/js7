package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.data.agent.RemoteTaskId
import java.net.InetSocketAddress

/**
 * @author Joacim Zschimmer
 */
final case class TaskStartArguments(
  remoteTaskId: RemoteTaskId,
  controllerAddress: InetSocketAddress,
  usesApi: Boolean,
  javaOptions: String,
  javaClasspath: String)
