package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.data.filebased.FileBasedsOverview
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class AgentsOverview(count: Int) extends FileBasedsOverview

object AgentsOverview extends FileBasedsOverview.Companion[Agent] {
  type Overview = AgentsOverview

  implicit val jsonCodec = deriveCodec[AgentsOverview]

  def fileBasedsToOverview(fileBaseds: Seq[Agent]) = AgentsOverview(fileBaseds.size)
}
