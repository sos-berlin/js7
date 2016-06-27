package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.taskserver.spoolerapi.TypedNamedIDispatches
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
private[task] final case class CommonArguments(
  agentTaskId: AgentTaskId,
  jobName: String,
  namedIDispatches: TypedNamedIDispatches,
  monitors: immutable.Seq[Monitor],
  hasOrder: Boolean,
  stdFiles: StdFiles)
