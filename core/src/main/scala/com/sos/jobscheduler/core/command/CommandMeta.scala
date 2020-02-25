package com.sos.jobscheduler.core.command

import com.sos.jobscheduler.base.auth.{SessionToken, SimpleUser}

/**
 * @author Joacim Zschimmer
 */
final case class CommandMeta(
  user: SimpleUser,
  sessionTokenOption: Option[SessionToken] = None)
