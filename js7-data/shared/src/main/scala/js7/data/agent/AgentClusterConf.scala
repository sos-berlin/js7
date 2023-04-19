package js7.data.agent

import js7.data.node.NodeId

object AgentClusterConf {
  // FIXME Should we use the SubagentId ?
  val primaryNodeId: NodeId = NodeId("Primary")
  val backupNodeId: NodeId = NodeId("Backup")
}
