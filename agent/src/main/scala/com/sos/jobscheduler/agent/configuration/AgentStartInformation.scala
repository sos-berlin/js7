package js7.agent.configuration

import js7.base.BuildInfo
import js7.base.time.Timestamp

/**
 * @author Joacim Zschimmer
 */
object AgentStartInformation {
  val StartedAt = Timestamp.now
  val PrettyVersion = BuildInfo.prettyVersion
  val BuildId = BuildInfo.buildId

  def initialize(): Unit = {}
}
