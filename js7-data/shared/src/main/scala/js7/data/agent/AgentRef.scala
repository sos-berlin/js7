package js7.data.agent

import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri

/**
  * @author Joacim Zschimmer
  */
final case class AgentRef(name: AgentName, uri: Uri)

object AgentRef
{
  implicit val jsonCodec = deriveCodec[AgentRef]
}
