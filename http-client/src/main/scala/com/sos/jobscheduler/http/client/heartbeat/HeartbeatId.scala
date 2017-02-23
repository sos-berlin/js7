package com.sos.jobscheduler.http.client.heartbeat

import com.sos.jobscheduler.base.generic.IsString
import java.util.UUID

/**
  * @author Joacim Zschimmer
  */
final case class HeartbeatId(string: String) extends IsString {
  require(string.isEmpty || !(string contains ' '), s"Invalid HeartbeatId: '$string'")

  override def toString = s"HeartbeatId($string)"
}

object HeartbeatId extends IsString.HasJsonFormat[HeartbeatId] {
  val Regex = """\p{Graph}+""".r

  def generate() = new HeartbeatId(UUID.randomUUID().toString)
}
