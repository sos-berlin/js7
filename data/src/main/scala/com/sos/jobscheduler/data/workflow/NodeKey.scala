package com.sos.jobscheduler.data.workflow

import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class NodeKey(workflowPath: WorkflowPath, nodeId: NodeId) {
  override def toString = s"${workflowPath.string}:${nodeId.string}"
}

object NodeKey {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
