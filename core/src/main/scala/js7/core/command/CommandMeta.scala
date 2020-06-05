package js7.core.command

import js7.base.auth.{SessionToken, SimpleUser}

/**
 * @author Joacim Zschimmer
 */
final case class CommandMeta(
  user: SimpleUser,
  sessionTokenOption: Option[SessionToken] = None)
