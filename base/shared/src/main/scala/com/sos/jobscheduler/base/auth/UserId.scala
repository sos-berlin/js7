package com.sos.jobscheduler.base.auth

import com.sos.jobscheduler.base.generic.GenericString

/**
  * @author Joacim Zschimmer
  */
final case class UserId(string: String) extends GenericString {
  import UserId._
  require(NameRegex.pattern.matcher(string).matches, s"Not a valid UserId: $string")
}

object UserId extends GenericString.Companion[UserId] {
  private val NameRegex = """^([\p{L}0-9_][\p{L}0-9_.-]*)?$""".r
  val Anonymous = UserId("Anonymous")
  val Empty = UserId("")
}
