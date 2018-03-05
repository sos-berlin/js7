package com.sos.jobscheduler.data

import com.sos.jobscheduler.data.filebased.FileBasedId

/**
  * @author Joacim Zschimmer
  */
package object agent {
  type AgentId = FileBasedId[AgentPath]
  val AgentId = new FileBasedId.Companion[AgentPath] {}
}
