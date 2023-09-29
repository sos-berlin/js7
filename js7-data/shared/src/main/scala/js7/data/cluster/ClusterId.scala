package js7.data.cluster

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder}
import js7.base.circeutils.CirceUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath

/** Identifies a Cluster.
 *
 * Only used where the Cluster must be named.
 */
trait ClusterId:
  def toTypedString: String

object ClusterId:
  implicit val jsonEncoder: Encoder[ClusterId] = _.toTypedString.asJson

  implicit val jsonDecoder: Decoder[ClusterId] = c =>
    c.value.asString match
      case None =>
        Left(DecodingFailure("String expected", c.history))

      case Some(string) =>
        if string.startsWith("Agent:") then
          AgentPath.checked(string.drop(6)).toDecoderResult(c.history)
        else
          Left(DecodingFailure(s"Unknown ClusterId: ${string.truncateWithEllipsis(20)}", c.history))
