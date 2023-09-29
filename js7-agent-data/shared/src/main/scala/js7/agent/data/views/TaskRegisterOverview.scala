package js7.agent.data.views

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/**
 * @author Joacim Zschimmer
 */
final case class TaskRegisterOverview(
  currentTaskCount: Int,
  totalTaskCount: Int)

object TaskRegisterOverview:
  implicit val jsonCodec: Codec.AsObject[TaskRegisterOverview] = deriveCodec
