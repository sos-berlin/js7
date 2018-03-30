package com.sos.jobscheduler.master.workflow

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.data.filebased.FileBasedsOverview
import com.sos.jobscheduler.data.workflow.Workflow
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowsOverview(count: Int) extends FileBasedsOverview

object WorkflowsOverview extends FileBasedsOverview.Companion[Workflow] {
  type Overview = WorkflowsOverview

  implicit val jsonCodec = deriveCodec[WorkflowsOverview]

  def fileBasedsToOverview(fileBaseds: Seq[Workflow]) = WorkflowsOverview(fileBaseds.size)
}
