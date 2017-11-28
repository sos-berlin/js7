package com.sos.jobscheduler.data.workflow

import io.circe.generic.JsonCodec

/**
 * @author Joacim Zschimmer
 */
@JsonCodec
final case class NodeKey(workflowPath: WorkflowPath, nodeId: NodeId) {
  override def toString = s"${workflowPath.string}:${nodeId.string}"
}
