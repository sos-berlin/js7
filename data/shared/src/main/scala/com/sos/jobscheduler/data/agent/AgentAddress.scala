package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.base.generic.GenericString
import java.net.URI

/**
  * The URL of the Agent. Should not end with a slash.
  *
  * @author Joacim Zschimmer
  */
final case class AgentAddress private(string: String) extends GenericString {
  require(!string.endsWith("/"), s"Invalid AgentAddress: $string")

  def requireURI(): Unit = new URI(string)  // Throws exception

  def toURI = new URI(string)
}

object AgentAddress extends GenericString.Checked_[AgentAddress]
{
  def unchecked(string: String) = new AgentAddress(string)

  def apply(uri: URI) = normalized(uri.toString)

  def normalized(string: String) = super.apply(
    if (string != "/") string stripSuffix "/" else string)
}
