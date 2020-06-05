package js7.data.agent

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.filebased.FileBasedsOverview

/**
  * @author Joacim Zschimmer
  */
final case class AgentsOverview(count: Int) extends FileBasedsOverview

object AgentsOverview extends FileBasedsOverview.Companion[AgentRef]
{
  type Overview = AgentsOverview

  implicit val jsonCodec = deriveCodec[AgentsOverview]

  def fileBasedsToOverview(fileBaseds: Seq[AgentRef]) = AgentsOverview(fileBaseds.size)
}
