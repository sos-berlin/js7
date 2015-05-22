package com.sos.scheduler.engine.agent.process

import com.google.inject.ImplementedBy
import com.sos.scheduler.engine.agent.data.commands.StartProcess

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[StandardAgentProcessFactory])
trait AgentProcessFactory extends (StartProcess ⇒ AgentProcess)
