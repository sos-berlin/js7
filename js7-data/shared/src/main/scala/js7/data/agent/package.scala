package js7.data

import js7.data.item.ItemId

/**
  * @author Joacim Zschimmer
  */
package object agent {
  type AgentRefId = ItemId[AgentRefPath]
  val AgentRefId = new ItemId.Companion[AgentRefPath] {}
}
