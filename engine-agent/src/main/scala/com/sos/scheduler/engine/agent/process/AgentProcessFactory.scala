package com.sos.scheduler.engine.agent.process

/**
 * @author Joacim Zschimmer
 */
object AgentProcessFactory extends (AgentProcessArguments ⇒ AgentProcess) {

  def apply(arguments: AgentProcessArguments): AgentProcess = arguments match {
    case o: DummyProcessArguments ⇒ new DummyAgentProcess(o)
    case o: DedicatedProcessArguments ⇒ new DedicatedAgentProcess(o)
  }
}
