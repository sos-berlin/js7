package js7.agent.data.views

import js7.base.circeutils.CirceCodec
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.system.SystemInformation
import js7.base.time.Timestamp
import js7.data.system.JavaInformation

/**
 * @author Joacim Zschimmer
 */
final case class AgentOverview(
  version: String,
  buildId: String,
  startedAt: Timestamp,
  isTerminating: Boolean,
  system: SystemInformation,
  java: JavaInformation)

object AgentOverview
{
  implicit val jsonCodec: CirceCodec[AgentOverview] = deriveCodec[AgentOverview]
}
