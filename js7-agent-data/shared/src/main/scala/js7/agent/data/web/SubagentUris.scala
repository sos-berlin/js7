package js7.agent.data.web

import js7.agent.data.web.SubagentUris.*
import js7.base.web.Uri

final class SubagentUris private(agentUri: Uri):

  private val prefixedUri = Uri(s"$agentUri/subagent")

  val overview: Uri = toUri("api")
  val session: Uri = toUri("api/session")
  val command: Uri = toUri("api/command")

  def apply(relativeUri: String): Uri =
    toUri(stripLeadingSlash(relativeUri))

  def api(relativeUri: String): Uri =
    if relativeUri.isEmpty then
      toUri("api")
    else
      toUri(s"api/${stripLeadingSlash(relativeUri)}")

  private def toUri(uri: String): Uri =
    prefixedUri / uri

  override def toString = agentUri.toString


object SubagentUris:
  def apply(uri: Uri): SubagentUris =
    var uri_ = uri.string
    if uri_.endsWith("subagent") then
      uri_ = uri_.stripSuffix("subagent")
    else if uri_.endsWith("subagent/") then
      uri_ = uri_.stripSuffix("subagent/")
    uri_ = uri_.stripSuffix("/")

    new SubagentUris(Uri(uri_))

  private def stripLeadingSlash(o: String) =
    o stripPrefix "/"
