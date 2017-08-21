package com.sos.jobscheduler.agent.command

import com.sos.jobscheduler.common.auth.User
import com.sos.jobscheduler.data.session.SessionToken

/**
 * @author Joacim Zschimmer
 */
final case class CommandMeta(
  user: User = User.Anonymous,
  sessionTokenOption: Option[SessionToken] = None)

object CommandMeta {
  private val Empty = new CommandMeta

  def apply(): CommandMeta = Empty
}
