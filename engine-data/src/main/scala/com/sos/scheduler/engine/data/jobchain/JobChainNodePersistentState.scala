package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.base.utils.HasKey

final case class JobChainNodePersistentState(
    jobChainPath: JobChainPath,
    nodeId: NodeId,
    action: JobChainNodeAction)
extends HasKey {

  type Key = JobChainNodePersistentStateKey

  def key = JobChainNodePersistentStateKey(jobChainPath, nodeId)
}
