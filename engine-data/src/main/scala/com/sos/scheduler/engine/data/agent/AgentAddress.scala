package com.sos.scheduler.engine.data.agent

import com.sos.scheduler.engine.base.generic.IsString
import java.net.URI

/**
  * The URL of the Agent. Should not end with a slash.
  *
  * @author Joacim Zschimmer
  */
final case class AgentAddress(string: String) extends IsString {
  require(!string.endsWith("/"), s"Invalid AgentAddress: $string")

  def requireURI(): Unit = new URI(string)  // Throws exception

  def toURI = new URI(string)
}

object AgentAddress extends IsString.Companion[AgentAddress] {
  def apply(uri: URI) = normalized(uri.toString)

  def normalized(string: String) = new AgentAddress(
    if (string != "/") string stripSuffix "/" else string)
}
