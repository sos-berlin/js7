package com.sos.scheduler.engine.common.auth

import com.sos.scheduler.engine.base.generic.IsString

/**
  * @author Joacim Zschimmer
  */
final case class UserId(string: String) extends IsString

object UserId extends IsString.Companion[UserId] {
  val Anonymous = UserId("Anonymous")
  val Empty = UserId("")
}
