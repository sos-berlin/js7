package com.sos.scheduler.engine.data.jobchain

import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class NodeKey(jobChainPath: JobChainPath, nodeId: NodeId) {
  override def toString = s"${jobChainPath.string}:${nodeId.string}"
}

object NodeKey {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
