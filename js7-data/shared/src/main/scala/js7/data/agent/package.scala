package js7.data

import js7.data.filebased.FileBasedId

/**
  * @author Joacim Zschimmer
  */
package object agent {
  type AgentRefId = FileBasedId[AgentRefPath]
  val AgentRefId = new FileBasedId.Companion[AgentRefPath] {}
}
