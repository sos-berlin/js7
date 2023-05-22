package js7.core.command

import js7.base.auth.SimpleUser

final case class CommandMeta(user: SimpleUser)

object CommandMeta {
  val System: CommandMeta = CommandMeta(SimpleUser.System)
}
