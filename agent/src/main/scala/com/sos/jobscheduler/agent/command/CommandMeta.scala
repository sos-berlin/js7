package com.sos.jobscheduler.agent.command

import com.sos.jobscheduler.base.auth.{SessionToken, SimpleUser}

/**
 * @author Joacim Zschimmer
 */
final case class CommandMeta(
  user: SimpleUser = SimpleUser.Anonymous,
  sessionTokenOption: Option[SessionToken] = None)

object CommandMeta {
  val Default = new CommandMeta
}
