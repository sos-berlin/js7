package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.data.filebased.FileBasedsOverview

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
