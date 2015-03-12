package com.sos.scheduler.engine.agent.commands

import com.sos.scheduler.engine.data.agent.AgentProcessId

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
trait ProcessCommand extends Command


trait StartProcess extends ProcessCommand {
  val controllerAddress: String
}

final case class StartThread(controllerAddress: String)
extends StartProcess

final case class StartDedicatedProcess(controllerAddress: String, javaOptions: String, javaClasspath: String)
extends StartProcess


/**
 * @author Joacim Zschimmer
 */
final case class StartProcessResponse(processId: AgentProcessId) extends Response


/**
 * @author Joacim Zschimmer
 */
final case class CloseProcess(processId: AgentProcessId, kill: Boolean)
extends ProcessCommand


/**
 * @author Joacim Zschimmer
 */
object CloseProcessResponse extends Response
