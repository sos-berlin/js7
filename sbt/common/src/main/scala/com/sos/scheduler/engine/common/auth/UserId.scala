package com.sos.scheduler.engine.common.auth

import com.sos.scheduler.engine.base.generic.IsString

/**
  * @author Joacim Zschimmer
  */
final case class UserId(string: String) extends IsString {
  import UserId._
  require(NameRegex.pattern.matcher(string).matches, s"Not a valid UserId: $string")
}

object UserId extends IsString.Companion[UserId] {
  private val NameRegex = """^([\p{L}0-9_][\p{L}0-9_.-]*)?$""".r
  val Anonymous = UserId("Anonymous")
  val Empty = UserId("")
}
