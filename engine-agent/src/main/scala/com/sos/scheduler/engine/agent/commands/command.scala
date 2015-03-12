package com.sos.scheduler.engine.agent.commands

import com.sos.scheduler.engine.data.agent.RemoteTaskId

/**
 * @author Joacim Zschimmer
 */
sealed trait Command


/**
 * @author Joacim Zschimmer
 */
sealed trait Response


/**
 * @author Joacim Zschimmer
 */
trait RemoteTaskCommand extends Command


trait StartRemoteTask extends RemoteTaskCommand {
  val controllerAddress: String
}

final case class StartRemoteInProcessTask(controllerAddress: String)
extends StartRemoteTask

final case class StartRemoteDedicatedProcessTask(controllerAddress: String, javaOptions: String, javaClasspath: String)
extends StartRemoteTask


/**
 * @author Joacim Zschimmer
 */
final case class StartRemoteTaskResponse(remoteTaskId: RemoteTaskId) extends Response


/**
 * @author Joacim Zschimmer
 */
final case class CloseRemoteTask(remoteTaskId: RemoteTaskId, kill: Boolean)
extends RemoteTaskCommand


/**
 * @author Joacim Zschimmer
 */
object CloseRemoteTaskResponse extends Response
