package com.sos.jobscheduler.data

import com.sos.jobscheduler.data.filebased.FileBasedId

/**
  * @author Joacim Zschimmer
  */
package object agent {
  type AgentRefId = FileBasedId[AgentRefPath]
  val AgentRefId = new FileBasedId.Companion[AgentRefPath] {}
}
