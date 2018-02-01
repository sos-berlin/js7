package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowsOverview(workflowCount: Int)

object WorkflowsOverview {
  implicit val jsonCodec = deriveCodec[WorkflowsOverview]
}
