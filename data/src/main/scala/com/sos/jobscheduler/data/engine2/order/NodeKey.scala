package com.sos.jobscheduler.data.engine2.order

import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
final case class NodeKey(jobnetPath: JobnetPath, nodeId: NodeId) {
  override def toString = s"${jobnetPath.string}:${nodeId.string}"
}

object NodeKey {
  implicit val MyJsonFormat = jsonFormat2(apply)
}
