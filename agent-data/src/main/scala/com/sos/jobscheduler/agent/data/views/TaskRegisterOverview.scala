package js7.agent.data.views

import io.circe.generic.JsonCodec

/**
 * @author Joacim Zschimmer
 */
@JsonCodec
final case class TaskRegisterOverview(
  currentTaskCount: Int,
  totalTaskCount: Int)
